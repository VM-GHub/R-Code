# This project is to predict whether a passanger on Titanic is survive or not
# survive from "test" data by using the evidence of "train" data.

# Load the simple data frames package, tibble
# install.packages("tibble")
library("tibble")

# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
# Apply add_column function from tibble library to add Survived column to the
# 2nd row at index 1 of the dataframe, test.
test.survived <- add_column(test, Survived = "None", .after = 1)

# Combine data sets
data.combined <- rbind(train, test.survived)
#*************************************************************

# A bit about R data types (e.g., factors), 
# str is the "structure of" and tells data type in the data frame.
str(data.combined)

# Turn Survived data type into a factor (bc 1 or 0 actually means
# survive or not survive, and not for numerical interest)
data.combined$Survived <- as.factor(data.combined$Survived)

# Turn pclass data type into a factor (bc 1,2,3 is the level of class)
data.combined$Pclass <- as.factor(data.combined$Pclass)
#*************************************************************

# Take a look at (the distribution) gross survival rates, 
# $ is take from a specific variable
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

# Load up ggplot2 package to use for visualizations
# install.packages("ggplot2")
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
# Turn Pclass in train into factor first
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")
#*************************************************************

# Examine the first few names in the training data set
head(as.character(train$Name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))
# There would be 1307 unique names, so it gives 2 duplicate names
# Number of duplicate name = Total(1309)-Unique(1307)

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]
#**************************************************************

# What is up with the 'Miss.' and 'Mr.' thing?  Maybe they were people with some predictive power?
library(stringr)

# Any correlation with other variables (e.g., SibSp?)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
# Take a first 5 rows in vector misses.  Here range index starts at 1 not 0
misses[1:5,]

#***************************************************************

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]
#***************************************************************

# Check out males to see if pattern continue
males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,]
#***************************************************************

# Expand upon the relationship between 'Survived' and 'Pclass' by adding the new 'Title' variable to the
# data set and then explore a potential 3-dimensional relationship

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0){
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0){
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0){
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0){
    return("Mr.")
  } else {
    return("Other")
  }
}

Titles <- NULL
for (i in 1:nrow(data.combined)) {
  # c function is to combine (or to concatenate), to a vector of expanding array
  Titles <- c(Titles, extractTitle(data.combined[i, "Name"]))
}

# Create the "Title" variable (or column) in data.combined and put the title values after
# converting them into factor and then into the variable (or column).
# Originally data.combined had 12 variables, and now it should have 13 variables
data.combined$Titles <- as.factor(Titles)

# Since we only have survived lables for the train set, only use the
# first 891 rows visualization
ggplot(data.combined[1:891,], aes(x = Titles, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
#***************************************************************

# What's the distribution of females to males across train & test?
table(data.combined$Sex)

# Visualize the 3-way relationship of Sex, Pclass, and Survival, compare to analyse
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")
#***************************************************************

# Ok, age and sex seem pretty important as derived from analysis of Title, let's try
# to look at the distributions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891, "Age")

# Plot Total count vs Survival Rates distribution in Age, Sex and Pclass
# Need to find a solution to graph or exclude the graph of missing data in Age column!
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  stat_count(width = 5) +
  facet_wrap(~Sex + Pclass)+
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")
#***************************************************************

# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$Titles == "Master."),]
summary(boys$Age)

# We know that "Miss." is moe complicated, let's examine further
misses <- data.combined[which(data.combined$Titles == "Miss."),]
summary(misses$Age)

# Need to find a solution to graph or exclude the graph of missing data in Age column!
ggplot(misses[misses$Survived !="None",], aes(x = Age, fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass)+
  xlab("Age") +
  ylab("Total Count") +
  ggtitle("Age for 'Miss.' by Pclass")
#****************************************************************

# Ok, it appears female children may have different survival rate,
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5)) # 14.5 is the max in boy age
#*****************************************************************

# Move on to the SibSp variable, summarize the variable
summary(data.combined$SibSp)

# can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive.  Visualize survival rates by SibSp, Pclass, and
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Titles)+
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0, 300) +
  ggtitle("Pclass, Title") +
  labs(fill = "Survived")
#******************************************************************

# Treat the Parch variable as a factor and visualize 
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Titles)+
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0, 300) +
  ggtitle("Pclass, Title") +
  labs(fill = "Survived")
#******************************************************************

# Let's try some feature engineering.  What about creating a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
# The add 1 is to say to include myself
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Titles)+
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0, 300) +
  ggtitle("Pclass, Title") +
  labs(fill = "Survived")
#*******************************************************************

# Take a look at the ticket variable
str(data.combined$Ticket)


# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize (to partition the graph)
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")
#*****************************************************************

# Next up - the fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))


# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")
#*****************************************************************

# Analysis of the cabin variable
str(data.combined$Cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Titles) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# What about folks with multiple cabins?
# Use str_detect to detect space " " in the multiple cabins
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Titles) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")
#*************************************************************

# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Titles) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")