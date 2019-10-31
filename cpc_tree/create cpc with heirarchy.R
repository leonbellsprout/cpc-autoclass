
# read in data
d <- read.csv("CPCSymbolList201908.csv")

# need to sort d on sort.key
attach(d)
d <- d[order(sort.key),]
detach(d)
# going to have to traverse the thing with a giant for loop?
head(d,25)

temp_symbol = 0
temp_symbol_last = 0
parents = rep(0, nrow(d))

for (i in 2:nrow(d)) {

	# store the previous symbol in temp_symbol
	temp_symbol <- as.character(d$SYMBOL[i-1])

	# if level is 7, the parent will just be the subclass
	if(d$level[i] == 7) {

		parents[i] <- substring(d$SYMBOL[i],1,4)

	# check if level is greater than previous level
	} else if(d$level[i] > d$level[i-1]) {

		# if so, parent should be previous symbol
		parents[i] <- temp_symbol
		# also store previous symbol in another variable 
		#temp_symbol_last <- temp_symbol

	# check if level is equal to the previous level
	} else if(d$level[i] == d$level[i-1]) {

		# if so, the parent symbol should be the saved temp_symbol_last
		# from the condition where level is greater than the previous level
		#parents[i] <- temp_symbol_last
		parents[i] <- parents[i-1]

	# this should be the only condition left: level is less than previous
	# level
	} else {
		
		# this one is going to be tricky
		# need to go back through the table and see if the current
		# level equals the next level going backwards
		# once they are equal, the parent symbol for that level
		# is going to be the same parent symbol for current level
		# ...
		# ...
		# ...that doesn't really make sense, but let's try anydangway
		j = 2
		while(d$level[i] != d$level[i-j]) {
			j = j + 1
		}
		# the parent of that one should now be current parent
		parents[i] <- parents[i-j]

	}

}

# merge parents into d 
d <- cbind(d,parents)

head(d,20)

write.csv(d,"cpc_with_heirarchy.csv")