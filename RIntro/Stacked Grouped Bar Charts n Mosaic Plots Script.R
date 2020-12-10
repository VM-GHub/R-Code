#Open file to choose LungCapDataTab.txt
LungCapData <- read.table(file.choose(), header = T, sep = "\t")

#The "V" in View is capitalized
View(LungCapData)

#Load LungCapData in current session memory to work with
attach(LungCapData)

#Show the categories of the data
names(LungCapData)

#Show class type and the data type in each class
class(Gender)
class(Smoke)
levels(Smoke)

#Display bar chart, this to examine relationship bwt 2 categorical variables
#Need to produce a contingency table first by using table commend before
#creating the bar chart
table(Smoke, Gender)

#Save contingent table to Table1 for use later, this table is 
#conditional probability
Table1 <- table(Smoke, Gender)
Table1

#Produce bar plot by using Table1, this is a stacked bar chart
barplot(Table1)

#Produce cluster bart chart
barplot(Table1, beside=T)

#Cluster bart chart with default legend
barplot(Table1, beside=T, legend.text = T)

#Customize legend and title by sending a vector to the legend argument
#and rotate y axis values, change bar color to red 2 and blue 4
barplot(Table1, beside=T, legend.text=c("Non-Smoke","Smoke"),
        main="Gender and Smoke", xlab="Gender", las=1, col=c(2,4))

#Use mosaic plots also to examine relationship bwt 2 categorical variables
mosaicplot(Table1)

#Clear session memory contents
detach(LungCapData)
