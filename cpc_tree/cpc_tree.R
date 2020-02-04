#######################################################
#
# filename: cpc_tree.R
# author: Jason Bell
# Created Date: 10/31/2019 HaPpY hAlLOwEEn!
#
# Description:
# This code will find the distance between cpc symbols 
# from two sources (e.g., machine, contractor)
# 
# Input files:
# CPCSymbolList201908 with parentage.csv
# Data file 
# 
# Output files:
# csv file containing distances for each 
# symbol within each source
# 
#
########################################################


# build tree structure for CPC scheme
# call forth any packages
library(data.table)
library(ggplot2)
library(foreach)
library(doSNOW)

# set working directory
# choose any file within the directory -- this will only keep the directory not the file
setwd(dirname(file.choose()))

# source function file
source("cpc_tree_functions.R")


#
# read in cpc symbol list
# locate CPCSymbolList201908 with parentage.csv when the prompt appears
# 
filename <- file.choose()
d.parents <- read.csv(filename)
head(d.parents)
d.parents$SYMBOL <- as.character(d.parents$SYMBOL)
d.parents$parents <- as.character(d.parents$parents)
d.parents$SYMBOL_NO_SPACE <- gsub(" ", "", d.parents$SYMBOL, fixed = TRUE)
d.parents$PARENTS_NO_SPACE <- gsub(" ", "", d.parents$parents, fixed = TRUE)

###
### test with random sample from Nelson
###

# Read in the data
# this needs to have the following columns: application id, cpc code, create user id, allocation type, and c-star indicator
# in that order, but not necessarily named the same 
#
# Note: if data comes from machine in multiple files, 
# use function catCPCFiles to read in
#
#
filename <- file.choose()
d.same <- data.table(read.csv(filename))

# this dataset has one cpc code from each application id
# remove these if this is the case and combine with 2nd dataset
#d.same <- d.same[Create.User.Id=="MACHINE"]

# if combining datasets, do so here
#filename2 <- file.choose()
#d.same2 <- data.table(read.csv(filename2, header=FALSE))

d.same
#d.same2

# read in using catCPCFiles function
d.same <- catCPCFiles()
d.same


# rename columns 
# this is done in the preprocessCPC function
# added "score" to names
#names(d.same) <- c("appl_id", "full_symbol_tx", "create_user_id", "allocation_type", "c_star","score")
#names(d.same2) <- c("appl_id", "create_user_id", "full_symbol_tx", "allocation_type", "c_star","score")

# only run this line if combining two datasets
#d.same <- rbind(d.same,d.same2)

# run this line to preprocess cpc data 
d.same <- preprocessCPC(d.same)

# examine dataset
d.same

unique(d.same[,appl_id])[1:20]

# use the following for testing functions on one application (or on a list thereof)
#d.same <- d.same[appl_id==15304390]

#d.same[source=="OTHER"]

# run through function getDistanceDT
# you will want to run the next 5 lines in case you step away and it finishes
# before you get back and the computer restartsssss
# you must have the directory created
#start_time <- Sys.time()

# write out dataset to share
write.csv(d.same,"C:\\Files\\Inbox\\R Output Files\\CPC random sample preprocessed 20191231.csv",row.names=FALSE)

d.output <- getDistanceDTNew(d.same) # took 8.5 hours for ~2000 apps
#end_time <- Sys.time()
#d.output <- cbind(d.same, d.distance.output) 
currentDate <- Sys.Date()
filename <- paste("C:\\Files\\Inbox\\R Output Files\\rand_samp_distance_",currentDate,".csv",sep="")
write.csv(d.output,filename,row.names=FALSE)

d.same
#d.distance.output
d.output
#d.output.merge <- merge(d.same,d.distance.output,by=c("appl_id","source","full_symbol_tx")) # giving too many rows
#write.csv(d.output,"difference_matrix.csv", row.names=FALSE)

#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=distance,x=SCORE))
#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=log(distance+.0001),x=SCORE))
#p + geom_point()

p <- ggplot(d.distance.output[source=="MACHINE"],aes(x=distance,fill=lineage))
p + geom_histogram()




# various testing
# test add this line to push/pull change to/from git repo
# test test test test

#d.temp <- getDistanceDT(d.same[appl_id=="15511761"])

#d.temp <- getDistanceDT(d.same[appl_id=="15736129"])
# merge instead of cbind
#merge(d.same,d.temp,by=c("appl_id","source","full_symbol_tx"))

#d <- d.same[appl_id=="15736129"]
#d <- d.same[appl_id=="15511761"]

#d.output[appl_id=="16326325"]
#d.output[appl_id=="15579374"]
#d.output[appl_id=="15511761"]

#d.output[,avg_distance:=mean(distance),by=list(appl_id, source)]
#d.output[distance<10000,avg_distance_no_sec:=mean(distance),by=list(appl_id, source)]
#length(unique(d.output[avg_distance_no_sec<25&source=="OTHER",appl_id]))


# see if apply function will work instead of the for loop

start_time <- Sys.time()
d.two_applications <- d.same[appl_id%in%c(13512421,13993183,15304182)]
d.two_applications[source=="MACHINE",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.two_applications[source=="OTHER",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]
d.two_applications[source=="OTHER",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.two_applications[source=="MACHINE",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]
end_time <- Sys.time()
print(end_time-start_time)

#d.two_applications <- d.same[appl_id%in%c(13512421,13993183,15304182,)]
d.two_applications <- d.same[appl_id%in%twenty_apps]
appl_list <- unique(d.two_applications[,appl_id])
d.output <- data.table(appl_id=numeric(0), full_symbol_tx=character(0),create_user_id=character(0),allocation_type=character(0), c_star=character(0), source=character(0), nsource=numeric(0), index=numeric(0), distance=numeric(0),lineage=logical(0))
start_time <- Sys.time()
for (id in appl_list) {
	d.one_application <- d.two_applications[appl_id==id]
	d.one_application[source=="MACHINE",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source=="OTHER",full_symbol_tx]),by=list(full_symbol_tx)]
	d.one_application[source=="OTHER",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source=="MACHINE",full_symbol_tx]),by=list(full_symbol_tx)]
	d.output <- rbind(d.output, d.one_application)
}
end_time <- Sys.time()
print(end_time-start_time)

d.one_application <- d.two_applications[appl_id==13512421]
d.one_application[source=="MACHINE",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source=="OTHER",full_symbol_tx]),by=list(full_symbol_tx)]
d.one_application[source=="OTHER",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source=="MACHINE",full_symbol_tx]),by=list(full_symbol_tx)]
rbind(d.output, d.one_application)

# try with more apps
twenty_apps <- unique(d.same[,appl_id])[1:20]
d.20apps <- d.same[appl_id%in%twenty_apps]

start_time <- Sys.time()
d.20apps[source=="MACHINE",c("distance","lineage","n_common_anc"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.20apps[source=="OTHER",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]
d.20apps[source=="OTHER",c("distance","lineage","n_common_anc"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.20apps[source=="MACHINE",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]
end_time <- Sys.time()
print(end_time-start_time)
d.20apps[appl_id==13512421]
# so I think that's going to work!


# so there was a giant file that got output in the interim
# check and see if that can load

# can't read this in -- too big 
# ah -- I see the problem. I was outputting the entire dataset
# each iteration, so the file became massive
# giantDT <- read.csv("C:\\Files\\Inbox\\R Output Files\\distance_function_int_dataset_2019-11-04.csv")

# see if can run faster using more cores?
#cores <- detectCores()
#cl <- makeCluster(cores[1]-1)
#cl <- makeCluster(2)
#registerDoSNOW(cl)
start_time <- Sys.time()
#outputDT <- data.table(appl_id=numeric(0), full_symbol_tx=character(0),create_user_id=character(0),allocation_type=character(0), c_star=character(0), source=character(0), nsource=numeric(0), index=numeric(0), distance=numeric(0),lineage=logical(0))
	
	outputDT <- rbindlist(
	foreach(i=1:20) %dopar% {
		d.one_application <- d.same[appl_id==twenty_apps[i]]
		d.one_application[source=="MACHINE",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source=="OTHER",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]
		d.one_application[source=="OTHER",c("distance","lineage"):=getMinDistanceLineage(appl_id, index, full_symbol_tx,d.one_application[source=="MACHINE",full_symbol_tx]),by=list(appl_id,full_symbol_tx)]

		#rbind(outputDT,d.one_application)

	}
	)



