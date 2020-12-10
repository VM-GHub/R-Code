#Open file to choose LungCapDataTab.txt
LungCapData <- read.table(file.choose(), header = T, sep = "\t")

#The "V" in View is capitalized
View(LungCapData)

#Load LungCapData in current session memory to work with
attach(LungCapData)

#Show the categories of the data
names(LungCapData)

#Extract the Lung Capacity, for only females and save in female LungCap
femaleLungCap <- LungCap[Gender=="female"]

#Display Stem plot for female lung capacity
stem(femaleLungCap)

#Adjust by scale
stem(femaleLungCap, scale=2)



#Clear session memory contents
detach(LungCapData)
