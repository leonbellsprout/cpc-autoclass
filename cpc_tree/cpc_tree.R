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
# CPCSymbolList201908 with parentage.csv (in Repository)
# Data file (not in Repository)
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

# rename columns 
names(d.same) <- c("appl_id", "full_symbol_tx", "create_user_id", "allocation_type", "c_star")

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

# machine is predicting subclass... get rid of any machine symbols 
# that aren't the full symbol
# machine predicted around 4300 symbols that aren't full symbols
d.same <- d.same[grep('/',full_symbol_tx)]

# sort dataset by application id, source, cpc code
d.same <- d.same[order(appl_id, source, full_symbol_tx)]

# examine dataset
d.same

# run through function getDistanceDT
# you will want to run the next 5 lines in case you step away and it finishes
# before you get back and the computer restartsssss
# you must have the directory created
start_time <- Sys.time()
d.distance.output <- getDistanceDT(d.same) # took 8.5 hours
end_time <- Sys.time()
d.output <- cbind(d.same, d.distance.output) 
write.csv(d.output,"C:\\Files\\Inbox\\R Output Files\\rand_samp_distance_20191029.csv",row.names=FALSE)

d.same
d.distance.output
d.output
#d.output.merge <- merge(d.same,d.distance.output,by=c("appl_id","source","full_symbol_tx")) # giving too many rows
#write.csv(d.output,"difference_matrix.csv", row.names=FALSE)

#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=distance,x=SCORE))
#p <- ggplot(d.output[distance&SOURCE=="MACHINE"],aes(y=log(distance+.0001),x=SCORE))
#p + geom_point()

p <- ggplot(d.distance.output[source=="MACHINE"],aes(x=distance,fill=lineage))
p + geom_histogram()




# various testing

#d.temp <- getDistanceDT(d.same[appl_id=="15511761"])

#d.temp <- getDistanceDT(d.same[appl_id=="15736129"])
# merge instead of cbind
#merge(d.same,d.temp,by=c("appl_id","source","full_symbol_tx"))

d <- d.same[appl_id=="15736129"]

d.output[appl_id=="16326325"]
d.output[appl_id=="15579374"]
d.output[appl_id=="15511761"]

#d.output[,avg_distance:=mean(distance),by=list(appl_id, source)]
#d.output[distance<10000,avg_distance_no_sec:=mean(distance),by=list(appl_id, source)]
#length(unique(d.output[avg_distance_no_sec<25&source=="OTHER",appl_id]))