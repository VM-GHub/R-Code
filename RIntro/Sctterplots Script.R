#Open file to choose LungCapDataTab.txt
LungCapData <- read.table(file.choose(), header = T, sep = "\t")

#The "V" in View is capitalized
View(LungCapData)

#Load LungCapData in current session memory to work with
attach(LungCapData)

#Show the categories of the data
names(LungCapData)

#Show class type and the 5 number summary in one of the classes
class(Age)
class(Height)
summary(Height)

#Display scatterplots that are appropriate for examining the relationship
#btw 2 numeric variables
#Before scatterplots, do a Pearson's correlation that is used to examine
#the strength of the linear relationship between the 2 numeric variables...
#The correlation will give a number is show whether the 2 numeric variables
#are closely related
cor(Age, Height)

#Now do plot(x, y)
plot(Age, Height)

#Customize legend and title by sending a vector to the legend argument
#and rotate y axis values, change plotting character 
#to be half of the original size using cex or pch, 
#change plotting character color using col for 4 blue
plot(Age, Height, main="Scatterplot", xlab="AGE", ylab="HEIGHT", las=1,
     xlim=c(0,25), cex=1, col=4)

#Add linear regression, which is a predicting line,  to fit to the scatterplot
#using abline(lm(y~x))
abline(lm(Height~Age))

#Customize the linear regression line
abline(lm(Height~Age), col=6)

#Add non-parametric smoother to describe the relation in observation
#and change line type using the lty arguement
#and change line width using the lwd arguement
lines(smooth.spline(Age, Height), lty=2, lwd=5)

#Clear session memory contents
detach(LungCapData)
