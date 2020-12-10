# plot a normal probability distribution,
# with mean=150, sd=40
plot(20:290, dnorm(20:290, mean=150, sd=40), type="l", xlab="X",
     ylab="Prob Density", main="Normal, Mean=150, SD=40", las=1)

# take a sample of size n from this population
n <- 20
samp <- rnorm(n, mean=150, sd=40)
hist(samp, prob=T)
mean(samp)
sd(samp)
# if you prefer, instead of a histogram, include a "smoothed
# density curve"
lines(density(samp), type="l")
# overlay the TRUE Normal probability distribution
lines(20:290, dnorm(20:290, mean=150, sd=40), type="l", col="blue")

### try submitting this code repeatedly, and observe how the
# histogram compares to the true probability distribution

####### now, use n=50

# now, try changing to take samples of n=50, and see what happens
n <- 50
samp <- rnorm(n, mean=150, sd=40)
hist(samp, prob=T)
mean(samp)
sd(samp)
# if you prefer, instead of a histogram, include a "smoothed
# density curve"
lines(density(samp), type="l")
# overlay the TRUE Normal probability distribution
lines(20:290, dnorm(20:290, mean=150, sd=40), type="l", col="blue")

### try submitting this code repeatedly, and observe how the
# histogram compares to the true probability distribution

##### now try using n=200

# now, try changing to take samples of n=200, and see what happens
n <- 200
samp <- rnorm(n, mean=150, sd=40)
hist(samp, prob=T)
mean(samp)
sd(samp)
# if you prefer, instead of a histogram, include a "smoothed
# density curve"
lines(density(samp), type="l")
# overlay the TRUE Normal probability distribution
lines(20:290, dnorm(20:290, mean=150, sd=40), type="l", col="blue")

### try submitting this code repeatedly, and observe how the
# histogram compares to the true probability distribution

##### NOW, watch the videos that follow this one, discussing 
##### the SAMPLING DISTRIBUTION....did you notice how the
##### sample mean varied around the true/population mean of 150
##### and, how the sample mean tended to stay closer to the 
##### true/population mean of 150 as n was increased.  Well, the
##### SAMPLING DISTRIBUTION describes this theoretical set of
##### "all possible samples means we could get", and the
##### STANDARD DEVIATION of the MEAN or STANDARD ERROR of the MEAN
##### describe, "on 'average', how far does a sample mean 
##### vary/deviate from the true/population mean
##### You can also play aroudn with the simulation we link to
##### to interactively learn about the SAMPLIGN DISTRIBUTION