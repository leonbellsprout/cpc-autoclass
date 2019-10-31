# build tree structure for CPC scheme
# call forth any packages
library(data.table)
library(ggplot2)

# set wd
setwd("C:\\Files\\Work\\CPC\\data")

# source function file
source("cpc_tree_functions.R")


#
# read in file from alteryx
# 

d.parents <- read.csv("CPCSymbolList201908 with parentage.csv")
head(d.parents)
d.parents$SYMBOL <- as.character(d.parents$SYMBOL)
d.parents$parents <- as.character(d.parents$parents)
d.parents$SYMBOL_NO_SPACE <- gsub(" ", "", d.parents$SYMBOL, fixed = TRUE)
d.parents$PARENTS_NO_SPACE <- gsub(" ", "", d.parents$parents, fixed = TRUE)


# functions are sourced above 
# test the function
#test <- getAncestry("A01G  13/0212")



#
# testing out a way to get distance, even across group, subclass, class, 
# section
#

# symbol1 <- "C07D519/00"
# symbol2 <- "C07D495/04"

# test
# test <- getSymbolDistance("A01G  13/0212", "A01G  13/0275")
# test

# test <- getSymbolDistance("A01G  13/0212", "A01G  13/0268")
# test

# test <- getSymbolDistance("A01G  13/0225", "A01G  13/0256")
# test

# test <- getSymbolDistance("A01G  13/0262", "A01G  13/0268")
# test

# sample 2 symbols at random and see what the eff happens
# s1 <- sample(d.parents$SYMBOL,1)
# s2 <- sample(d.parents$SYMBOL,1)

# test <- getSymbolDistance(s1, s2)
# test
# try with two symbols on an app
# s1 <- "G16H  10/60"
# s2 <- "G16H  10/65"

# s1 <- "G16H  40/67"
# s2 <- "G16H  40/63"
# s2 <- "G16H  40/60"
# s2 <- "G16H  40/00"

# s1 <- "G16H  20/17"
# s2 <- "G16H  20/10"

# s1 <- 'A01B   1/06'
# s2 <- 'A01B   1/00'

# test <- getSymbolDistance(s1, s2)
# test

# something wrong with function,
# so test some parts of it
# s1 <- "A01G  13/0212"
# s2 <- "A01G  13/0275"
# s2 <- "A01G  13/0206"
# getAncestry(s1)
# getAncestry(s2)
# test <- getSymbolDistance(s1,s2)
# test


# read in some more datar
# this dataset has two applications that were reviewed by both 
#  contractors. look to see the "distance" between them

# d.same <- read.csv("same_apps.csv")
# d.same$FULL_SYMBOL_TX <- as.character(d.same$FULL_SYMBOL_TX)
# d.same$FULL_SYMBOL_TX <- gsub(" ", " ", d.same$FULL_SYMBOL_TX)
# d.same$CREATE_USER_ID <- as.character(d.same$CREATE_USER_ID)
# attach(d.same)
# d.same <- d.same[order(APPLICATION_ID, CREATE_USER_ID),]
# detach(d.same)
# d.same

# #s1 <- "A63B  37/0062"
# s1 <-  "H01L  29/7869"
# s2 <- "A63B  37/0063"

# getAncestry(s1)

# test <- getSymbolDistance(s1,s2)
# test

# # create an empty data frame to hold distance and lineage values
# d.distance <- data.frame(distance=0,lineage=0)

# # create list of applications
# appl_list <- unique(d.same$APPLICATION_ID)
# counter <- 1
# for(i in 1:length(appl_list)) {
# 	# get data for just one application
# 	d.one_application <- d.same[d.same$APPLICATION_ID==appl_list[i],]
# 	# get the different user names (should only be 2 (for now))
# 	user_list <- unique(d.one_application$CREATE_USER_ID)
# 	# get the symbols for each user name
# 	symbol_set_1 <- d.one_application$FULL_SYMBOL_TX[d.one_application$CREATE_USER_ID == user_list[1]]
# 	symbol_set_2 <- d.one_application$FULL_SYMBOL_TX[d.one_application$CREATE_USER_ID == user_list[2]]
# 	# nested loop and get distance and lineage for each symbol pair
# 	for(k in 1:length(symbol_set_1)) {
# 		# temp distance vector
# 		outer_symbol_distance <- data.frame(distance=0,lineage=0)

# 		for(l in 1:length(symbol_set_2)) {

# 			if(symbol_set_1[k] == symbol_set_2[l]) {
# 				outer_symbol_distance[l,] <- c(0, 1)
# 			} else {
# 				temp_distance <- getSymbolDistance(symbol_set_1[k], symbol_set_2[l])
# 				outer_symbol_distance[l,] <- c(temp_distance$distance, temp_distance$lineage)
# 			}
# 		}
# 		print(outer_symbol_distance)

# 		# do some logic to get the closest distance
# 		# first check if any of the distances are lineage==TRUE
# 		if(length(which(outer_symbol_distance$lineage==1)) == 1) {
# 			t <- which(outer_symbol_distance$lineage==1)
# 			min_distance <- outer_symbol_distance$distance[t]
# 			d.distance[counter,] <- c(min_distance,TRUE)
# 		} else if(length(which(outer_symbol_distance$lineage==1)) > 1) {
# 			t <- which(outer_symbol_distance$lineage==1)
# 			min_distance <- min(outer_symbol_distance$distance)[t]
# 			d.distance[counter,] <- c(min_distance,TRUE)
# 		} else {
# 			min_distance <- min(outer_symbol_distance$distance)
# 			d.distance[counter,] <- c(min_distance,FALSE)
# 		}

# 		counter <- counter+1
# 	}

# 	for(k in 1:length(symbol_set_2)) {
# 		# temp distance vector
# 		outer_symbol_distance <- data.frame(distance=0,lineage=0)

# 		for(l in 1:length(symbol_set_1)) {

# 			if(symbol_set_2[k] == symbol_set_1[l]) {
# 				outer_symbol_distance[l,] <- c(0, 1)
# 			} else {
# 				temp_distance <- getSymbolDistance(symbol_set_2[k], symbol_set_1[l])
# 				outer_symbol_distance[l,] <- c(temp_distance$distance, temp_distance$lineage)
# 			}
# 		}
# 		print(outer_symbol_distance)

# 		# do some logic to get the closest distance
# 		# first check if any of the distances are lineage==TRUE
# 		if(length(which(outer_symbol_distance$lineage==1)) == 1) {
# 			t <- which(outer_symbol_distance$lineage==1)
# 			min_distance <- outer_symbol_distance$distance[t]
# 			d.distance[counter,] <- c(min_distance,TRUE)
# 		} else if(length(which(outer_symbol_distance$lineage==1)) > 1) {
# 			t <- which(outer_symbol_distance$lineage==1)
# 			min_distance <- min(outer_symbol_distance$distance)[t]
# 			d.distance[counter,] <- c(min_distance,TRUE)
# 		} else {
# 			min_distance <- min(outer_symbol_distance$distance)
# 			d.distance[counter,] <- c(min_distance,FALSE)
# 		}

# 		counter <- counter+1
# 	}

# }

# d.same
# d.distance

# d.output <- cbind(d.same, d.distance)
#write.csv(d.output,"difference_matrix.csv", row.names=FALSE)




###
### test with sample from Nelsonian
###

#d.same <- read.csv("cpc tree distance sample 20190926.csv")
d.same <- data.table(read.csv("EVAL FULL 101719.csv"))
names(d.same) <- c("appl_id", "full_symbol_tx", "create_user_id", "allocation_type", "c_star")

d.same[,full_symbol_tx := as.character(full_symbol_tx)]
d.same[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
d.same[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
d.same[,create_user_id := as.character(create_user_id)]
# get rid of create_user_id == "CO-OWN-BACKFILE"
d.same <- d.same[create_user_id!="CO-OWN-BACKFILE"]
# get rid of Y symbols
d.same <- d.same[substring(full_symbol_tx,1,1)!="Y"]
d.same[,source := sapply(create_user_id, function(x) if (x=="MACHINE") {"MACHINE"} else {"OTHER"})]

# machine is predicting subclass... get rid of any machine symbols 
# that aren't the full symbol
# machine predicted around 4300 symbols that aren't full symbols

d.same <- d.same[grep('/',full_symbol_tx)]


d.same <- d.same[order(appl_id, source, full_symbol_tx)]

#d.same[,id:=seq(1,.N,by=1)]

d.same

#s1 <- "A63B  37/0062"
#s1 <-  "H01L  29/7869"
#s2 <- "A63B  37/0063"

#getAncestry(s1)

#test <- getSymbolDistance(s1,s2)
#test

# run through function getDistanceDT

start_time <- Sys.time()
d.distance.output <- getDistanceDT(d.same) # took 8.5 hours
end_time <- Sys.time()

d.same
d.distance.output

d.output <- cbind(d.same, d.distance.output) 
d.output
d.output.merge <- merge(d.same,d.distance.output,by=c("appl_id","source","full_symbol_tx")) # giving too many rows
#write.csv(d.output,"difference_matrix.csv", row.names=FALSE)

#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=distance,x=SCORE))
#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=log(distance+.0001),x=SCORE))
#p + geom_point()

p <- ggplot(d.distance.output[source=="MACHINE"],aes(x=distance,fill=lineage))
p + geom_histogram()


write.csv(d.output,"rand_samp_distance_20191029.csv",row.names=FALSE)

#d.temp <- getDistanceDT(d.same[appl_id=="15511761"])

d.temp <- getDistanceDT(d.same[appl_id=="15736129"])
# merge instead of cbind
merge(d.same,d.temp,by=c("appl_id","source","full_symbol_tx"))

d <- d.same[appl_id=="15736129"]

d.output[appl_id=="16326325"]
d.output[appl_id=="15579374"]
d.output[appl_id=="15511761"]

d.output[,avg_distance:=mean(distance),by=list(appl_id, source)]
d.output[distance<10000,avg_distance_no_sec:=mean(distance),by=list(appl_id, source)]
length(unique(d.output[avg_distance_no_sec<25&source=="OTHER",appl_id]))