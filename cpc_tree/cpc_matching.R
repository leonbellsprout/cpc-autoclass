#######################################################
#
# filename: cpc_matching.R
# author: Jason Bell
# Created Date: 11/14/2019 
#
# Description:
# This will calculate matches between the machine
# and the contractor at the symbol, main group, and 
# subclass levels. 
# 
# Input files:
# Data file 
# 
# Output files:
# csv file containing matches
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
# filename <- file.choose()
# d.parents <- read.csv(filename)
# head(d.parents)
# d.parents$SYMBOL <- as.character(d.parents$SYMBOL)
# d.parents$parents <- as.character(d.parents$parents)
# d.parents$SYMBOL_NO_SPACE <- gsub(" ", "", d.parents$SYMBOL, fixed = TRUE)
# d.parents$PARENTS_NO_SPACE <- gsub(" ", "", d.parents$parents, fixed = TRUE)

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
d.same <- d.same[Create.User.Id=="MACHINE"]

# if combining datasets, do so here
filename2 <- file.choose()
d.same2 <- data.table(read.csv(filename2, header=FALSE))

d.same
d.same2

# rename columns 
names(d.same) <- c("appl_id", "full_symbol_tx", "create_user_id", "allocation_type", "c_star")
names(d.same2) <- c("appl_id", "create_user_id", "full_symbol_tx", "allocation_type", "c_star")

d.same <- rbind(d.same,d.same2)

# strip whitespace from symbol text
d.same[,full_symbol_tx := as.character(full_symbol_tx)]
d.same[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
d.same[,full_symbol_tx := gsub(" ", "", full_symbol_tx)]
d.same[,create_user_id := as.character(create_user_id)]

# get rid of create_user_id == "CO-OWN-BACKFILE"
d.same <- d.same[create_user_id!="CO-OWN-BACKFILE"]

# get rid of Y symbols
d.same <- d.same[substring(full_symbol_tx,1,1)!="Y"]

# create a new column which will create two sources from multiple (as long as one is named "MACHINE")
d.same[,source := sapply(create_user_id, function(x) if (x=="MACHINE") {"MACHINE"} else {"OTHER"})]

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

# add subclass, maingroup
d.same[,subclass:=substr(full_symbol_tx,1,4)]
d.same[,maingroup:=gsub('/.*',"",full_symbol_tx)]

d.same

# need to write a function to loop through and get 
# counts for how many symbols are in the same subclass, main group

# create a list of application numbers that and loop through list
# using data.table speed
appl_list <- unique(d.same[,appl_id])
currentTime <- Sys.time()
logfile <- paste("C:\\Files\\Inbox\\R Output Files\\count_matching_log_",format(currentTime,"%Y%m%d_%H%M%S"),".txt",sep="")
filename <- paste("C:\\Files\\Inbox\\R Output Files\\count_matching_int_dataset_",format(currentTime,"%Y%m%d_%H%M%S"),".csv",sep="")
sink(logfile, append=TRUE)
outputDT <- rbindlist(
	foreach(i=1:length(appl_list)) %dopar% {
		source1 <- unique(d.same[,source])[1]
		source2 <- unique(d.same[,source])[2]
	#for (id in appl_list) {
		# get data for one application
		#d.one_application <- d[appl_id==id]
		d.one_application <- d.same[appl_id==appl_list[i]]

		# get min distance and lineage for each direction
		# machine -> other
		# other -> machine
		d.one_application[source==source1,c("match","match_subcls","match_maingrp"):=getCountMatch(appl_id, index, full_symbol_tx,d.one_application[source==source2,full_symbol_tx],i),by=list(full_symbol_tx)]
		# write each iteration in case of system failure
		#write.table(d.one_application, filename, sep = ",", col.names = !file.exists(filename), append = T,row.names=FALSE)
		# append to output dataset
		#outputDT <- rbind(outputDT, d.one_application)

		# print progress to file
		#n <- n + 1
		#cat(paste("Iteration:",i,Sys.time(),appl_id))
		#Sys.sleep(0.01)
		#flush.console()
		write.table(d.one_application, filename, sep = ",", col.names = !file.exists(filename), append = T,row.names=FALSE)
	}
	)
sink()
write.csv(outputDT[source=="MACHINE"],"C:\\Files\\Inbox\\R Output Files\\count_subclass_maingroup.csv",row.names=FALSE)

test_app <- 16291503
d.one_application <- d.same[appl_id==test_app]
symbol_set <- d.one_application[source=="OTHER",full_symbol_tx]
i <- 1
j <- 11
symbol <- d.one_application[create_user_id=="MACHINE",full_symbol_tx][j]

match <- 0
	match_subcls <- 0
	match_maingrp <- 0

	# loop through symbol set to find matches
	# if a match is found, updated match variables 
	# and exit loop
	for(i in 1:length(symbol_set)) {
		print(i)
	
		if(symbol==symbol_set[i]) {
			match <- 1
			match_maingrp <- 1
			match_subcls <- 1
			break
		} else if(gsub('/.*',"",symbol)==gsub('/.*',"",symbol_set[i])) {
			match_maingrp <- 1
			match_subcls <- 1
			
		} else if(substr(symbol,1,4)==substr(symbol_set[i],1,4)) {
			match_subcls <- 1
			
		}

			 	
	}