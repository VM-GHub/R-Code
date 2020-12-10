# we can use the write.table command to export data, to many formats
?write.table

# save the file in our current working directory, name it 
# "ExportedFileName", and save as a .CSV file format
write.table(DataToExport, file="ExportedFileName.csv", sep=",")

# export without row names by setting the "row.names=FALSE"
# also note, that this will over-write the previous file
write.table(DataToExport, file="ExportedFileName.csv", row.names=F, sep=",")

# specify the path for where to save the file instead
write.table(DataToExport, 
            file="/Users/OldMarin/TEACHING/FolderToSaveIn/ExportedFileName.csv",
            row.names=F, sep=",")

# write.csv does the same, just dont need to specify sep=","
write.csv(DataToExport, 
          file="/Users/OldMarin/TEACHING/FolderToSaveIn/ExportedFileName.csv", 
          row.names=F)

# save as tab-delim txt file, setting sep="\t" and file extension to .txt
write.table(DataToExport, 
            file="/Users/OldMarin/TEACHING/FolderToSaveIn/ExportedFileName.txt", 
            row.names=F, sep="\t")

# or, save it as space-delimited by setting sep=" "
write.table(DataToExport, 
            file="/Users/OldMarin/TEACHING/FolderToSaveIn/ExportedFileNameSpace", 
            row.names=F, sep=" ")
