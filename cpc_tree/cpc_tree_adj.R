#######################################################
#
# filename: cpc_tree_adj.R
# author: Jason Bell
# Created Date: 11/14/2019 
#
# Description:
# This code will find the distance between cpc symbols 
# from two sources (e.g., machine, contractor)
# edit 11/14/2019: in this case, between US and KR
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

# rename columns 
#names(d.same) <- c("appl_id", "full_symbol_tx", "create_user_id", "allocation_type", "c_star")
names(d.same) <- c("appl_id", "source", "subclass", "group", "subgroup", "full_symbol_tx")
#names(d.same2) <- c("appl_id", "create_user_id", "full_symbol_tx", "allocation_type", "c_star")

#d.same <- rbind(d.same,d.same2)

# strip whitespace from symbol text
d.same[,full_symbol_tx := as.character(full_symbol_tx)]
d.same[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
d.same[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
d.same[,source := as.character(source)]

# get rid of create_user_id == "CO-OWN-BACKFILE"
#d.same <- d.same[create_user_id!="CO-OWN-BACKFILE"]

# get rid of Y symbols
#d.same <- d.same[substring(full_symbol_tx,1,1)!="Y"]

# create a new column which will create two sources from multiple (as long as one is named "MACHINE")
#d.same[,source := sapply(create_user_id, function(x) if (x=="MACHINE") {"MACHINE"} else {"OTHER"})]

# get rid of applications with < 2 sources
d.same<-d.same[,nsource:=length(unique(source)),by=appl_id][nsource>1]

# machine is predicting subclass... get rid of any machine symbols 
# that aren't the full symbol
# machine predicted around 4300 symbols that aren't full symbols
d.same <- d.same[grep('/',full_symbol_tx)]

# sort dataset by application id, source, cpc code
d.same <- d.same[order(appl_id, source, full_symbol_tx)]

# this dataset has a bunch of dups -- remove them!
d.same <- unique(d.same, by=c("appl_id", "source", "full_symbol_tx"))

# add application index for keeping track in loop
d.same[,index:=1:.N]

# examine dataset
d.same

#unique(d.same[,appl_id])[1:20]
#d.same[appl_id==13512421]

#d.same[source=="OTHER"]

# run through function getDistanceDT
# you will want to run the next 5 lines in case you step away and it finishes
# before you get back and the computer restartsssss
# you must have the directory created
#start_time <- Sys.time()
d.output <- getDistanceDTNew(d.same) # took 8.5 hours for ~2000 apps
#end_time <- Sys.time()
#d.output <- cbind(d.same, d.distance.output) 
#currentDate <- Sys.Date()
#filename <- paste("C:\\Files\\Inbox\\R Output Files\\rand_samp_distance_",currentDate,".csv",sep="")
#write.csv(d.output,filename,row.names=FALSE)

#d.same
#d.distance.output
#d.output
#d.output.merge <- merge(d.same,d.distance.output,by=c("appl_id","source","full_symbol_tx")) # giving too many rows
#write.csv(d.output,"difference_matrix.csv", row.names=FALSE)

#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=distance,x=SCORE))
#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=log(distance+.0001),x=SCORE))
#p + geom_point()

#p <- ggplot(d.distance.output[source=="MACHINE"],aes(x=distance,fill=lineage))
#p + geom_histogram()




# various testing

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