# tableau is out, so in the mean time, do some graphics here
library(ggplot2)
library(data.table)
library(MASS)

# read in data
filename <- file.choose()
d <- data.table(read.csv(filename))

# look at the distance vs score
p <- ggplot(d,aes(y=distance,x=score))
p + geom_point()

# that's bizarre
p <- ggplot(d[distance<100],aes(distance,x=score))
p + geom_point()

# write a function to return density
get_density <- function(x, y, ...) {
	dens <- kde2d(x, y, ...)
	ix <- findInterval(x, dens$x)
	iy <- findInterval(x, dens$y)
	ii <- cbind(ix, iy)
	return(dens$z[ii])
}

# now get the density for the data
d$density <- get_density(d$score, d$distance)

# plot with density
p <- ggplot(d[distance<100&source=="MACHINE"],aes(y=distance,x=score, colour=density))
p + geom_point()

# histogram of score
p <- ggplot(d,aes(x=score))
p + geom_histogram()

#
# score is weird
# let's try rank instead
#

# get the density for the data
d$density <- get_density(d$Rank, d$distance)

# plot with density
p <- ggplot(d[distance<100],aes(y=distance,x=Rank, colour=density))
p + geom_point()
