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
	d.distance <- data.table(appl_id=numeric(), source=character(),full_symbol_tx=character(), distance=numeric(), lineage=logical())

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


# function to get minimum distance and lineage
# between a symbol and a list of symbols
getMinDistanceLineage <- function(symbol,symbol_set) {

	# define a data.table to hold all distance and lineage
	outer_symbol_distance <- data.table(distance=numeric(0),lineage=logical(0))

	min_distance <- 0
	min_lineage <- FALSE

	for(i in 1:length(symbol_set)) {
		# print(paste("Iteration: ",i, ", ss1, Inner: ", l))
		# # flush console
		# Sys.sleep(0.01)
		# flush.console()

		if(symbol == symbol_set[i]) {
			#print(paste("Iteration: ",i, ", ss1, Inner: ", l))
			# flush console
			# Sys.sleep(0.01)
			# flush.console()
			# print(paste("Iteration:",i, "Skipping: Same Symbol Found",symbol,symbol_set[i],"distance: 0"))
			# Sys.sleep(0.01)
			# flush.console()
			outer_symbol_distance <- rbindlist(list(outer_symbol_distance, list(0,TRUE)), use.names=FALSE)

			# if the symbols match, then no need to continue
			break
		
		} else {
			temp_distance <- getSymbolDistance(symbol, symbol_set[i])
			
			# also get the number of rows currently in 
			# outer_symbol_distance
			outer_symbol_distance.nrow <- nrow(outer_symbol_distance)

			# if this is the first symbol being evaluated, can't
			# compare to previous symbol
			if(outer_symbol_distance.nrow>1) {

				# if the distance is bigger than the previous 
				# distance, no need to write it
				if (temp_distance$distance>=outer_symbol_distance[outer_symbol_distance.nrow-1,distance]&temp_distance$lineage==FALSE) {
					# print(paste("Iteration:",i,symbol,symbol_set[i],"distance:",temp_distance$distance))
					# Sys.sleep(0.01)
					# flush.console()
					#print("Skipping: Current Distance > Previous Distance")
					next

				} else {
			# 		print(paste("Iteration:",i,symbol,symbol_set[i],"distance:",temp_distance$distance))
			# 		Sys.sleep(0.01)
			# flush.console()
					outer_symbol_distance <- rbindlist(list(outer_symbol_distance, list(temp_distance$distance,temp_distance$lineage)), use.names=FALSE)
				}

			} else {
			# 	print(paste("Iteration:",i,symbol,symbol_set[i],"distance:",temp_distance$distance))
			# 	Sys.sleep(0.01)
			# flush.console()
				outer_symbol_distance <- rbindlist(list(outer_symbol_distance, list(temp_distance$distance,temp_distance$lineage)), use.names=FALSE)
			}
		}

	}

	# do some logic to get the closest distance
	
	if(length(which(outer_symbol_distance$lineage==1)) >= 1) {
		
		t <- which(outer_symbol_distance$lineage==1)
		min_distance <- min(outer_symbol_distance$distance[t])
		min_lineage <- TRUE
		
		#d.distance <- rbindlist(list(d.distance, list(min_distance,TRUE)), use.names=FALSE)
	} else {
		
		min_distance <- min(outer_symbol_distance$distance)
		
		#d.distance <- rbindlist(list(d.distance, list(min_distance,FALSE)), use.names=FALSE)
	}

	# now return min_distance and min_lineage
	return(list(min_distance=min_distance, min_lineage=min_lineage))
	
	

}
		
			

			
				

				
		