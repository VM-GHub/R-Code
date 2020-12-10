# Using the 'tAPPLY' function in R

# read in the "LungCapData.csv" data, and attach it
LungCapData <- read.table(file="~/Desktop/R Videos Project/LungCapData.txt", sep="\t", header=T)

# check the data
summary(LungCapData)
# and attach it
attach(LungCapData)

# get the help menu
?tapply

# calculate the mean Age for Smoker/NonSmoker
tapply(X=Age, INDEX=Smoke, FUN=mean, na.rm=T)

# you don't need to include "X", "INDEX",... as long as you
# ehter them in that order...
# we also don't need to include "na.rm=T" as no missing values
tapply(Age, Smoke, mean)

# we can save the output in a new "object"
m <- tapply(Age, Smoke, mean)
m

# also worth discussing is the use of the "SIMPLIFY" argument
# this is set to TRUE by default...if we set it to "FALSE"...
tapply(Age, Smoke, mean, simplify=FALSE)

# note that we could get the same using [ ], 
# although using "tapply" is more efficient
mean(Age[Smoke=="no"])
mean(Age[Smoke=="yes"])

# let's look at applying the "summary" function to groups
tapply(Age, Smoke, summary)

# or, applying the "quantile" function to the groups
tapply(Age, Smoke, quantile, probs=c(0.2, 0.8))

# we can "subset" based on multiple variables/vectors
# calculate the mean Age for Smoker/NonSmoker and male/female
tapply(X=Age, INDEX=list(Smoke, Gender), FUN=mean, na.rm=T)

# a less efficient way to get this done...
mean(Age[Smoke=="no" & Gender=="female"])
mean(Age[Smoke=="no" & Gender=="male"])
mean(Age[Smoke=="yes" & Gender=="female"])
mean(Age[Smoke=="yes" & Gender=="male"])

# a reminder of using 2 grouping variables
tapply(Age, list(Smoke, Gender), mean, na.rm=T)

# an a note that the "by" function is the same as tapply, 
# except it presents the results similar to a vector
by(Age, list(Smoke, Gender), mean, na.rm=T)

# and we can subset the elements in the usual way
temp <- by(Age, list(Smoke, Gender), mean, na.rm=T)
temp
temp[4]
# and see the "class" of temp
class(temp)
# we can also convert it to a vector if we prefer
c(temp)
temp2 <- c(temp)
temp2
# and check it's class
class(temp2)
