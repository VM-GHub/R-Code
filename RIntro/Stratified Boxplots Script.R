#Open file to choose LungCapDataTab.txt
LungCapData <- read.table(file.choose(), header = T, sep = "\t")

#The "V" in View is capitalized
View(LungCapData)

#Load LungCapData in current session memory to work with
attach(LungCapData)

#Create a "AgeGroups" variable
AgeGroups <- cut(Age, breaks=c(0,13,15,17,25), label=c("<13", "14/15", "16/17", "18+"))

#Check the first 5 Age, and AgeGroups
Age[1:5]

#AgeGoups divide the Age category into 4 groups or strata (singular is stratum)
AgeGroups[1:5]

#Show the levels or categories of AgeGroups
levels(AgeGroups)

#Create a boxplot of the LungCap
boxplot(LungCap, ylab="Lung capacity", main="Boxplot of LungCap", las=1)

#Create a boxplot for smoker and non-smoker seperately 
# (divide LungCap by the Smoke category, using ~ to seperate data)
boxplot(LungCap~Smoke, ylab="Lung capacity", main="LungCap vs Smoke", las=1)

#Create a boxplot for Lungcap vs Smoke only for whose age is
# equal to or greater than 18 (using [] to subset data)
boxplot(LungCap[Age>=18]~Smoke[Age>=18], ylab="Lung capacity", 
        main="LungCap vs Smoke, for the 18+", las=1)

#Visualize the relationship between LungCapacity and Smoking
# within each of the Age Strata.
# That is, create boxplots of Lung Capacity for Smokers and Non-Smokers for:
# 13 or younger, 14-15, 16-17, 18 or older
# The * creates a combination of categories which is the product of
# the number of category in Smoke times the number of category in AgeGroups
# that's 2x4, or 8 total combinations
boxplot(LungCap~Smoke*AgeGroups, ylab="Lung capacity", 
        main="LungCap vs Smoke, by AgeGroups", las=2)

#Boxplot w colours
boxplot(LungCap~Smoke*AgeGroups, ylab="Lung capacity", 
        main="LungCap vs Smoke, by AgeGroups", las=2, col=c(4,2))

#Make the nice plot, with changed x-axis names, legend,...
boxplot(LungCap~Smoke*AgeGroups, ylab="Lung capacity", 
        main="LungCap vs Smoke, by AgeGroups", las=2, col=c("blue","red"),
        axes=F, xlab="Age Strata")

#Add a box around it
box()

#Relabel the y-axis
axis(2, at=seq(0,20,2), seq(0,20,2), las=1)

#Relabel the x-axis
axis(1, at=c(1.5,3.5,5.5,7.5), labels=c("<13","14-15","16-17","18+"))

#Add legend
legend(x=5.5, y=4.5, legend=c("Non-Smoke","Smoke"), col=c(4,2),
       pch=15, cex=0.8)

#Clear session memory contents
detach(LungCapData)