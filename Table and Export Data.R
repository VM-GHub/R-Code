#Help for write.table
?write.table

#Create a table from a matrix
smoke <- matrix(c('*',51,43,22,'*',92,28,21,'*',68,22,9),ncol=4,byrow=TRUE)
colnames(smoke) <- c("*","High","Low","Middle")
rownames(smoke) <- c("current","former","never")
#Display table
smoke

#Export table data as csv file using write.table and is saved to current
#working directory C:\Users\UserName\Documents
write.table(smoke, file="smoke.csv",sep=",")

#Specify the path for where to save the file instead
write.table(smoke, file="/Users/UserName/Documents/R Dataset for R Tutorials/smoke.csv",sep=",")
