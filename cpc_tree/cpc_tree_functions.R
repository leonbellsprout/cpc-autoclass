#######################################################
#
# filename: cpc_tree_functions.R
# author: Jason Bell
# Created Date: 10/31/2019 HaPpY hAlLOwEEn!
#
# Description:
# The source code containing functions necessary
# for cpc_tree.R
# 
# Input files:
# NA
# 
# Output files: 
# NA
# 
#
########################################################


# write a function that will get the dot level of a symbol, 
# then create an empty ancestry vector, pass that to the new function

getAncestry <- function(symbol) {

	# not going back to the zero dot
	# fixed 9/10/2019
	# was only iterating i-1 times, 

	# get the row for the symbol passed in
	#t <- d.parents$SYMBOL==symbol
	t <- d.parents$SYMBOL_NO_SPACE==symbol
	# get the dot level
	max_i <- d.parents$dot_level[t]
	#print(paste("Number deep:",max_i))

	if (length(max_i) == 0) {
		return(NA)
	} else if (max_i == 0) {
		#return(d.parents$SYMBOL[t])
		return(d.parents$SYMBOL_NO_SPACE[t])
	}

	# pass that stuff into a function to get the ancestry stuff
	ancestry <- funGetAncestors(symbol, 1, max_i, rep(0, max_i))
    ancestry <- c(symbol, ancestry)
	

	# return the ancestry vector
	#print(ancestry)
	return(ancestry)
}

funGetAncestors <- function(symbol, i, max_i, ancestry) {
	#print(paste("iteration", i))
	# escape if max row
	if (i == max_i+1) {
		#print(ancestry)
		return(ancestry)
	}
	# get row
	#t <- d.parents$SYMBOL==symbol
	t <- d.parents$SYMBOL_NO_SPACE==symbol
	# get parent
	#ancestry[i] <- d.parents$parents[t]
	ancestry[i] <- d.parents$PARENTS_NO_SPACE[t]
	#print(ancestry[i])
	return(funGetAncestors(ancestry[i], i+1, max_i, ancestry))
}

# now create a function to take two symbols, get the ancestry, 
# then need to find out the common parent and count how far apart
# they are. 
# 
# I think "distance" will be to count from one symbol to the 
# common parent, then count to the other symbol

getSymbolDistance <- function(symbol1, symbol2) {

	# get ancestry for each symbol
	s1_anc <- getAncestry(symbol1)
	s2_anc <- getAncestry(symbol2)

	# now need to compare if there are any ancestors
	#commonParent <- unlist(sapply(s1_anc,function(x)which(x%in%s2_anc)))[1]
	#commonParent <- names(commonParent)

	commonParents <- unlist(sapply(s1_anc,function(x)which(x%in%s2_anc)))
	commonParents <- names(commonParents)

	commonParent <- commonParents[1]
	# get the element number of the matched parent for each 
	# ancestry vector
	distance <- 0
	lineage <- FALSE
	# if there is a common parent, get the distance
	if(!is.null(commonParent)) {
		#d1 <- which(s1_anc==commonParent)
		#d2 <- which(s2_anc==commonParent)
		#print (paste("d1:",d1))
		#print (paste("d2:",d2))
		# if the common parent is the first in the list,
		# that means they're siblings

		# note: compare ancestry -- eliminate the common symbols
		# count what is left, and that should be the distance

			#if (d1 == 2 & d2 == 2) {
			#	distance <- 1
			#} else {

				# sum up the symbols the two ancestry vectors do not
				# have in common
				#distance <- sum(d1, d2)
				not_in_common_1 <- !(s1_anc%in%commonParents)
				not_in_common_2 <- !(s2_anc%in%commonParents)

				# check if any anc set
				if(class(s1_anc)=="logical" | class(s2_anc)=="logical") {
					distance <- NA
				} else {
					distance <- sum(not_in_common_1, not_in_common_2)
				}

				# lineage is if the two symbols are in the same lineage, 
				# i.e. one is not a sibling or uncle
				if (symbol1%in%commonParents | symbol2%in%commonParents) {
					lineage <- TRUE
				} else {
					lineage <- FALSE
				}
			#}

	# otherwise, it gets a little trickier
		# just return a value 999
		# there's no way to compare across different sections, for 
		# example. If there were no symbols in the same main group, 
		# it's just wrong:)
	} else {

		#parentLocation1 <- which(d.parents$SYMBOL==s1_anc[length(s1_anc)])
		#parentLocation2 <- which(d.parents$SYMBOL==s2_anc[length(s2_anc)])

		#distance = abs(parentLocation1 - parentLocation2) + length(s1_anc) + length(s2_anc)
		#return(list(distance=999,lineage=FALSE))


		# need to find out at what point they differ,
		# i.e., do they differ only at the main group, or the subclass?

		# first, sum the nodes in the ancestries
		distance <- sum(length(s1_anc),length(s2_anc))

		# get the string before the slash
		# match at the main group? Obviously not
		# don't even do this one, because if they match at this, 
		# they would have a common ancestor, so it wouldn't 
		# ever be true
		#sub("\\/.*", "", symbol1)==sub("\\/.*", "", symbol2)

		# next, see what point they differ
		# match at the subclass level?
		if (substring(symbol1,1,4)==substring(symbol2,1,4)) {
			distance <- distance + 10
		
		# match at the class level?
		} else if (substring(symbol1,1,3)==substring(symbol2,1,3)) {
			distance <- distance + 100
		# match at the section level?
		} else if (substring(symbol1,1,2)==substring(symbol2,1,2)) {
			distance <- distance + 1000
		} else {
			distance <- distance + 10000
		}
	}

	# need to take into account if parent/sibling relationship
	# a parent/child relationship is closer than two siblings
	# a parent->child is 0, a grandparent->grandchild = 1

	# need to also return how far down in the hierarchy
	# total distance vs lineage distance
	# 

	return(list(symbol1=symbol1, symbol2=symbol2, ancestry1=s1_anc, ancestry2=s2_anc, commonParent=commonParent,commonParents=commonParents, distance=distance, logDistance=log(distance), lineage=lineage))

}



#
# write function to return distance matrixy thingy
#


getDistanceDT <- function(d) {
	#require(svMisc)
	# create an empty data frame to hold distance and lineage values	
	d.distance <- data.table(appl_id=numeric(), source=character(),full_symbol_tx=character(), distance=numeric(), lineage=logical(), n_common_anc=numeric())

	# make sure d has only 2 unique sources
	if(length(unique(d[,source]))!=2) {
		print("ERROR: There were not exactly 2 sources")
		# flush console
		Sys.sleep(0.01)
		flush.console()
		return
	}

	# make sure symbols have no spaces
	d[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
	d[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]

	# make sure dataset it sorted
	#d <- d[order(appl_id, source, full_symbol_tx)]

	# create list of applications
	appl_list <- unique(d[,appl_id])
	counter <- 1
	
	for(i in 1:length(appl_list)) {
		print(paste("Counter:", counter))
		print(paste("Iteration:",i, "Application:",appl_list[i]))
		# flush console
		Sys.sleep(0.01)
		flush.console()
		# get data for just one application
		d.one_application <- d[appl_id==appl_list[i],]
		# get the different user names (should only be 2 (for now))
		user_list <- unique(d.one_application[,source])
		# get the symbols for each user name
		symbol_set_1 <- d.one_application[source==user_list[1],full_symbol_tx]
		symbol_set_2 <- d.one_application[source==user_list[2],full_symbol_tx]

		# if one of the symbol sets is empty, e.g., no data
		# from one of the two sources, need to skip iteration
		if(length(symbol_set_1) == 0 | length(symbol_set_2)==0) {
			
			# create a sequence for all the counter values that will be
			# set to null in d.distance
			#counter_seq <- seq(counter,counter+max(length(symbol_set_1),length(symbol_set_2)),by=1)
			counter_seq <- max(length(symbol_set_1),length(symbol_set_2))

			# set d.distance = null for those symbols
			#d.distance[counter_seq,c("distance","lineage") := list(NA,NA)]
			src <- "MACHINE"
			symbols <- symbol_set_1
			if (length(symbol_set_1)==0) {
				src <- "OTHER"
				symbols <- symbol_set_2
			} 

			d.distance <- rbindlist(list(d.distance, list(rep(appl_list[i],counter_seq),rep(src,counter_seq),symbols,rep(NA,counter_seq),rep(FALSE,counter_seq))), use.names=FALSE)
			
			# update counter variable
			counter <- counter + counter_seq #+ 1

			# print out warning message
			print(paste("Skipping ", counter_seq, " symbols for ", appl_list[i]))

			# flush console
			Sys.sleep(0.01)
			flush.console()
			# skip to next iteration
			next
		}

		# nested loop and get distance and lineage for each symbol pair
		# for MACHINE source
		for(k in 1:length(symbol_set_1)) {
			#print(paste("Iteration: ",i, ", ss1, Outer: ", k))
			# flush console
			#Sys.sleep(0.01)
			#flush.console()

			# call getMinDistanceLineage to get the minimum distance
			# for each symbol in symbol_set_1 compared to symbol_set_2
			#
			# function returns a list to update d.distance
			min_distance_lineage <- getMinDistanceLineage(symbol_set_1[k],symbol_set_2)

			#print(paste("Application:",appl_list[i],"Source: MACHINE","Symbol",symbol_set_1[k],"Min Distance:",min_distance_lineage$min_distance,"Lineage:",min_distance_lineage$min_lineage))

			d.distance <- rbindlist(list(d.distance, list(appl_list[i],"MACHINE",symbol_set_1[k],min_distance_lineage$min_distance,min_distance_lineage$min_lineage)), use.names=FALSE)

			

			counter <- counter+1
		}

		# same for OTHER source
		for(k in 1:length(symbol_set_2)) {
			#print(paste("Iteration: ",i, ", ss2, Outer: ", k))
			# flush console
			#Sys.sleep(0.01)
			#flush.console()

			# call getMinDistanceLineage to get the minimum distance
			# for each symbol in symbol_set_1 compared to symbol_set_2
			#
			# function returns a list to update d.distance
			min_distance_lineage <- getMinDistanceLineage(symbol_set_2[k],symbol_set_1)

			#print(paste("Application:",appl_list[i],"Source: OTHER","Symbol",symbol_set_2[k],"Min Distance:",min_distance_lineage$min_distance,"Lineage:",min_distance_lineage$min_lineage))

			#d.distance <- rbindlist(list(d.distance, min_distance_lineage), use.names=FALSE)
			d.distance <- rbindlist(list(d.distance, list(appl_list[i],"OTHER",symbol_set_2[k],min_distance_lineage$min_distance,min_distance_lineage$min_lineage)), use.names=FALSE)

			

			counter <- counter+1
		}

		# flush console
		#Sys.sleep(0.01)
		#flush.console()

	}

	return(d.distance)
}

# create a more efficient version
getDistanceDTNew <- function(d) {
	#require(svMisc)
	# create an empty data frame to hold distance and lineage values	
	#d.distance <- data.table(appl_id=numeric(), source=character(),full_symbol_tx=character(), distance=numeric(), lineage=logical())

	# create the output dataset -- empty dataset to append
	#outputDT <- data.table(appl_id=numeric(0), full_symbol_tx=character(0),create_user_id=character(0),allocation_type=character(0), c_star=character(0), source=character(0), nsource=numeric(0), index=numeric(0), distance=numeric(0),lineage=logical(0))

	
	#d[source=="MACHINE",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d[source=="OTHER",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]
	#d[source=="OTHER",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d[source=="MACHINE",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]

	# create a list of application numbers that and loop through list
	# using data.table speed
	appl_list <- unique(d[,appl_id])

	# create filename with current date
	currentTime <- Sys.time()
	filename <- paste("C:\\Files\\Inbox\\R Output Files\\distance_function_int_dataset_",format(currentTime,"%Y%m%d_%H%M%S"),".csv",sep="")
	logfile <- paste("C:\\Files\\Inbox\\R Output Files\\distance_log_",format(currentTime,"%Y%m%d_%H%M%S"),".txt",sep="")

	# need to get both sources from data so they aren't hard coded
	source1 <- unique(d[,source])[1]
	source2 <- unique(d[,source])[2]

	# get start time
	start_time <- Sys.time()
	# print system message
	print(paste("Beginning distance function at:",start_time))
	Sys.sleep(0.01)
	flush.console()
	n <- 1
	sink(logfile, append=TRUE)
	# loop through each application
	outputDT <- rbindlist(
	foreach(i=1:length(appl_list)) %dopar% {
	#for (id in appl_list) {
		# get data for one application
		#d.one_application <- d[appl_id==id]
		d.one_application <- d[appl_id==appl_list[i]]

		# get min distance and lineage for each direction
		# machine -> other
		# other -> machine
		d.one_application[source==source2,c("distance","lineage","n_common_anc"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source==source1,full_symbol_tx],i),by=list(full_symbol_tx)]
		d.one_application[source==source1,c("distance","lineage","n_common_anc"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source==source2,full_symbol_tx],i),by=list(full_symbol_tx)]
		# write each iteration in case of system failure
		write.table(d.one_application, filename, sep = ",", col.names = !file.exists(filename), append = T,row.names=FALSE)
		# append to output dataset
		#outputDT <- rbind(outputDT, d.one_application)

		# print progress to file
		#n <- n + 1
		#cat(paste("Iteration:",i,Sys.time(),appl_id))
		#Sys.sleep(0.01)
		#flush.console()
	}
	) # end rbindlist call
	# get end time
	sink()
	end_time <- Sys.time()
	# print system messages
	print(paste("Finished at:",end_time))
	print(paste("Total time:",end_time-start_time))



	

	#return(d.distance)
	return(outputDT)
}


# function to get minimum distance and lineage
# between a symbol and a list of symbols
getMinDistanceLineage <- function(appl_id, index, symbol, symbol_set,counter) {
	# define a data.table to hold all distance and lineage
	outer_symbol_distance <- data.table(distance=numeric(0),lineage=logical(0),n_common_anc=numeric(0))
	#print(paste("Current symbol:",symbol))
	#print("Outer Symbol Matrix initialized")
	#print(outer_symbol_distance)
	#Sys.sleep(0.01)
	#flush.console()

	min_distance <- 0
	min_lineage <- FALSE

	for(i in 1:length(symbol_set)) {
		#print(paste("Iteration: ",i, ", ss1, Inner: ", l))
		#print(paste("Beginning Loop:",i))
		# # flush console
		#Sys.sleep(0.01)
		#flush.console()

		if(symbol == symbol_set[i]) {
			#print(paste("Iteration: ",i, ", ss1, Inner: ", l))
			# flush console
			#Sys.sleep(0.01)
			#flush.console()
			#print(paste("Iteration:",i, "Skipping: Same Symbol Found",symbol,symbol_set[i],"distance: 0"))
			#Sys.sleep(0.01)
			#flush.console()
			#outer_symbol_distance <- rbindlist(list(outer_symbol_distance, list(0,TRUE)), use.names=FALSE)

			# if the symbols match, then no need to continue
			#break
			return(list(min_distance=0, min_lineage=TRUE,n_common_anc=0))
		
		} else {
			temp_distance <- getSymbolDistance(symbol, symbol_set[i])
			# print this out
			#print(paste("distance:",temp_distance$distance,"lineage:",temp_distance$lineage))
			#Sys.sleep(0.01)
			#flush.console()
			
			# also get the number of rows currently in 
			# outer_symbol_distance
			outer_symbol_distance.nrow <- nrow(outer_symbol_distance)
			#print(paste("outer_symbol_distance nrow =",outer_symbol_distance.nrow))
			#Sys.sleep(0.01)
			#flush.console()
			# if this is the first symbol being evaluated, can't
			# compare to previous symbol
			if(outer_symbol_distance.nrow<1) {

				#print(paste("Iteration:",i,symbol,symbol_set[i],"distance:",temp_distance$distance))
			 	#Sys.sleep(0.01)
				#flush.console()
				outer_symbol_distance <- rbindlist(list(outer_symbol_distance, list(temp_distance$distance,temp_distance$lineage,length(temp_distance$commonParents))), use.names=FALSE)

				

			} else {
				#print("outer_symbol_distance >= 1")
				#Sys.sleep(0.01)
				#flush.console()

				# two main cases
				# the previous row has lineage == TRUE or FALSE

				if(outer_symbol_distance[outer_symbol_distance.nrow,lineage]==TRUE) {

					#print("previous row lineage == TRUE")
					#Sys.sleep(0.01)
					#flush.console()

					# now, if current row has lineage == FALSE or distance is >= previous row, skip it
					if(temp_distance$lineage==FALSE | temp_distance$distance>=outer_symbol_distance[outer_symbol_distance.nrow,distance]) {
						#print("current lineage == FALSE or current distance >= previous distance")
						#Sys.sleep(0.01)
						#flush.console()
						next
					# if not, that means current distance is less than
					# previous and current lineage is TRUE
					# can replace previous with current
					} else {
						#print("current distance < previous and current lineage == TRUE")
						#print("Removing previous row")
						#Sys.sleep(0.01)
						#flush.console()
						outer_symbol_distance <- data.table(distance=as.numeric(temp_distance$distance),lineage=temp_distance$lineage,n_common_anc=length(temp_distance$commonParents))
					}

				# if not, previous row lineage == FALSE
				} else {
					#print("previous row lineage == FALSE")
					#Sys.sleep(0.01)
					#flush.console()

					# if current row lineage is TRUE or distance is < 
					# previous row, replace previous row
					if(temp_distance$lineage==TRUE | temp_distance$distance<outer_symbol_distance[outer_symbol_distance.nrow,distance]) {
						#print("current lineage == TRUE or current distance < previous distance")
						#print("Removing previous row")
						#Sys.sleep(0.01)
						#flush.console()
						outer_symbol_distance <- data.table(distance=as.numeric(temp_distance$distance),lineage=temp_distance$lineage,n_common_anc=length(temp_distance$commonParents))

					# if not, skip
					} else {
						#print("current distance >= previous distance and current lineage == FALSE")
						#Sys.sleep(0.01)
						#flush.console()
						next
					}

				}


				# if the distance is bigger than the previous 
				# distance, no need to write it
				#if (temp_distance$distance>=outer_symbol_distance[outer_symbol_distance.nrow-1,distance]&temp_distance$lineage==FALSE) {
					# print(paste("Iteration:",i,symbol,symbol_set[i],"distance:",temp_distance$distance))
					# Sys.sleep(0.01)
					# flush.console()
					#print("Skipping: Current Distance > Previous Distance")
				#	next

				#} else if(temp_distance$distance<outer_symbol_distance[outer_symbol_distance.nrow-1,distance]&temp_distance$lineage==FALSE) {




				#}else {
			# 		print(paste("Iteration:",i,symbol,symbol_set[i],"distance:",temp_distance$distance))
			# 		Sys.sleep(0.01)
			# flush.console()
				#	outer_symbol_distance <- rbindlist(list(outer_symbol_distance, list(temp_distance$distance,temp_distance$lineage)), use.names=FALSE)
				#}
			 	
			}
		}

	}

	# at this point, the outer_symbol_distance matrix should
	# only have one row, which SHOULD be the minimum

	#print(paste("Finished outer loop for symbol",symbol))
	#print(outer_symbol_distance)
	#print(class(outer_symbol_distance$distance))
	#print(class(outer_symbol_distance$lineage))


	# # do some logic to get the closest distance
	
	# if(length(which(outer_symbol_distance$lineage==1)) >= 1) {
		
	# 	t <- which(outer_symbol_distance$lineage==1)
	# 	min_distance <- min(outer_symbol_distance$distance[t])
	# 	min_lineage <- TRUE
		
	# 	#d.distance <- rbindlist(list(d.distance, list(min_distance,TRUE)), use.names=FALSE)
	# } else {
		
	# 	min_distance <- min(outer_symbol_distance$distance)
		
	# 	#d.distance <- rbindlist(list(d.distance, list(min_distance,FALSE)), use.names=FALSE)
	# }

	# if((i%%20)==0){
	 print(paste("Iteration:",counter,Sys.time(),appl_id, "Index:", index, symbol))
	# Sys.sleep(0.01)
	# flush.console()
	# }
	# now return min_distance and min_lineage
	#return(list(min_distance=min_distance, min_lineage=min_lineage))
	return(list(min_distance=outer_symbol_distance$distance, min_lineage=outer_symbol_distance$lineage,n_common_anc=outer_symbol_distance$n_common_anc))
	
	

}
		
getCountMatch <- function(appl_id, index, symbol, symbol_set,counter) {
	# define a data.table to hold all distance and lineage
	#outer_symbol_distance <- data.table(distance=numeric(0),lineage=logical(0))
	#print(paste("Current symbol:",symbol))
	#print("Outer Symbol Matrix initialized")
	#print(outer_symbol_distance)
	#Sys.sleep(0.01)
	#flush.console()

	#min_distance <- 0
	#min_lineage <- FALSE

	match <- 0
	match_subcls <- 0
	match_maingrp <- 0

	# loop through symbol set to find matches
	# if a match is found, updated match variables 
	# and exit loop
	for(i in 1:length(symbol_set)) {
	
		if(symbol==symbol_set[i]) {
			match <- 1
			match_maingrp <- 1
			match_subcls <- 1
			break
		} else if(gsub('/.*',"",symbol)==gsub('/.*',"",symbol_set[i])) {
			match_maingrp <- 1
			match_subcls <- 1
			#break
		} else if(substr(symbol,1,4)==substr(symbol_set[i],1,4)) {
			match_subcls <- 1
			#break
		}

			 	
	}
		

	
	print(paste("Iteration:",counter,Sys.time(),appl_id, "Index:", index, symbol))
	

	#return(list(min_distance=min_distance, min_lineage=min_lineage))
	return(list(match=match, match_subcls=match_subcls, match_maingrp=match_maingrp))
	
	

}			

# function to concatenate files
catCPCFiles <- function() {
# load libraries
	require(tcltk2)
	require(data.table)

	# choose filenames
	filenames <- tk_choose.files()

	# create empty data.table
	data.machine <- data.table()
	data.contractor <- data.table()

	for(filename in filenames) {

		if(grepl("contractor", filename)) {
			data.contractor <- rbind(data.contractor, as.data.table(read.csv(filename)))
		} else if(grepl("machine", filename)) {
			data.machine <- rbind(data.machine, as.data.table(read.csv(filename)))
		}
	}

	data.machine[,Create.User.Id:="MACHINE"]
	data.machine[,Allocation.Type.Code:=NA]
	data.machine[,C.Star.Indicator:=NA]
	data.contractor[,Algorithm.Score:=1]

	# combine machine and contractor data
	return(rbind(data.machine[,list(Application.Number,Cpc.Code,Create.User.Id,Allocation.Type.Code,C.Star.Indicator,Algorithm.Score)],data.contractor[,list(Application.Number,Cpc.Code,Create.User.Id,Allocation.Type.Code,C.Star.Indicator,Algorithm.Score)]))

# end function
}

# function to preprocess cpc data
preprocessCPC <- function(data) {
	# rename columns
	names(data) <- c("appl_id", "full_symbol_tx", "create_user_id", "allocation_type", "c_star","score")

	# strip whitespace from symbol text
	data[,full_symbol_tx := as.character(full_symbol_tx)]
	data[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
	data[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
	data[,create_user_id := as.character(create_user_id)]

	n1 <- nrow(data)
	# get rid of create_user_id == "CO-OWN-BACKFILE"
	data <- data[create_user_id!="CO-OWN-BACKFILE"]

	print(paste("There were",n1-nrow(data),"with CO-OWN-BACKFILE as create_user_id removed."))

	n1 <- nrow(data)
	# get rid of Y symbols
	data <- data[substring(full_symbol_tx,1,1)!="Y"]
	print(paste("There were",n1-nrow(data),"Y symbols removed."))

	# create a new column which will create two sources from multiple (as long as one is named "MACHINE")
	data[,source := sapply(create_user_id, function(x) if (x=="MACHINE") {"MACHINE"} else {"OTHER"})]

	n1 <- nrow(data)
	# get rid of applications with < 2 sources
	data<-data[,nsource:=length(unique(source)),by=appl_id][nsource>1]
	print(paste("There were",n1-nrow(data),"rows removed due to application only having one source."))	

	# machine is predicting subclass... get rid of any machine symbols 
	# that aren't the full symbol
	# machine predicted around 4300 symbols that aren't full symbols
	data <- data[grep('/',full_symbol_tx)]

	# sort dataset by application id, source, cpc code
	data <- data[order(appl_id, source, full_symbol_tx)]

	n1 <- nrow(data)
	# this dataset has a bunch of dups -- remove them!
	data <- unique(data, by=c("appl_id", "source", "full_symbol_tx"))
	print(paste("There were",n1-nrow(data),"duplicates removed."))

	# add application index for keeping track in loop
	data[,index:=1:.N]

	# this line is used to fix an error when printing 
	# the data.table after function call ends
	data[]

	# return data
	return(data)
}
