#This script gives numeric summary for categorical and numeric variable.
#The numeric summary can contain mean, standard deviation, frequencies and more.
#It is often of the interest to quantify the centre and spread of the
#distribution of a variable...

#Open file to choose LungCapDataTab.txt
LungCapData <- read.table(file.choose(), header = T, sep = "\t")

#The "V" in View is capitalized
View(LungCapData)

#Load LungCapData in current session memory to work with
attach(LungCapData)

#Show the categories of the data
names(LungCapData)

#Ask for summary for the variable LungCap
summary(LungCap)

#Ask for summaries for categorical variable on Smoke and
#for numeric variable on Lung Capacity

#Starting with categorical variable: Smoke.
#Categorical variables are summarized using a Frequency or a Proportion
#Use table to display frequency
#Divide the table by the sample population gives the proportion
#Use length commend to get the the sample size in a category
table(Smoke)
table(Smoke)/725
length(Smoke)
table(Smoke)/length(Smoke)

#To produce a contingent table (2-way table), enter 2 variables into the table
table(Smoke, Gender)

#Now move to create table for numerica variable: Lung Capacity.
#Ask for the numeric mean
mean(LungCap)
#Ask for a trimmed mean, by removing top 10% and bottom 10% of the popuplation
mean(LungCap, trim=0.10)
#Calculate the median
median(LungCap)
#Calculate the variance
var(LungCap)
#Calculate the standard deviation
sd(LungCap)
#Calculate the standard deviation by taking the square root of the variance.
sqrt(var(LungCap))
#Calculate the variance by squaring the standard deviation
sd(LungCap)^2
#Calculate minimum
min(LungCap)
#Calculate maximum
max(LungCap)
#Calculate range
range(LungCap)
#Calculate quantile or percentile
quantile(LungCap, probs=0.9)
#Calculate multiple percentile
quantile(LungCap, probs=c(0.20, 0.5, 0.9,1))
#Calculate the sum
sum(LungCap)
#Calculate the mean by using sum
sum(LungCap)/length(LungCap)
#Calculate correlation between lung capacity and age
cor(LungCap, Age)
#Calculate spearman correlation between lung capacity and age
cor(LungCap, Age, method="spearman")
#Calculate covariance btw lung capacity and age
cov(LungCap, Age)
#Calculate variance btw lung capacity and age
var(LungCap, Age)

#Summary for numeric variable: Lung Capacity
summary(LungCap)
#Summary for categorical variable: Smoke
summary(Smoke)
#Summary fot the entire data set
summary(LungCapData)



#Clear session memory contents
detach(LungCapData)
