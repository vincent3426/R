###############################################################################
# FileName : FindSameFiles.R
# Author : Sanchez Vincent
# Email : vincent3426@bupt.edu.cn
# Date : 2016-01-13
# Notice : 
# 1.Find the same files in a directory which can contain both files and directories.
# 2.Count the quantity of the files and the unique files in a directory.
# 3.Count the total size of a directory.
###############################################################################

library(tools)
rm(list = ls())

dirPath <- choose.dir()
cnt <<- 0
tmp <<- list()
filePath <<- list()
folderSize <<- 0
md5 <- array()
md5Index <- 0
PATH <- list()

# FilePath -------------------------------------------------------

### arg0 pass the path of directory
FilePath <- function(arg0){
  dirPath <- arg0
  cnt <<- cnt + 1
  lf <- list.files(dirPath)
  if(substr(dirPath, nchar(dirPath), nchar(dirPath))=="\\"){
    tmp[[cnt]] <<- paste(dirPath, lf, sep = "")
  }else{
    tmp[[cnt]] <<- paste(dirPath, "\\", lf, sep = "")
    dirIndex <- which(file.info(tmp[[cnt]])$isdir == TRUE)
    fileIndex <- which(file.info(tmp[[cnt]])$isdir == FALSE)
    # if no dir
    if(length(dirIndex) != 0){
      filePath[[cnt]] <<- tmp[[cnt]][-dirIndex]
      for(path in tmp[[cnt]][dirIndex]){
        FilePath(path)
      }
    }
    # if no file
    else if(length(fileIndex) != 0){
      filePath[[cnt]] <<- tmp[[cnt]]
    }
  }
}

# CountFolderSize -------------------------------------------------------

CountFolderSize <- function(){
  for(i in 1:length(filePath)){
    if(length(filePath[[i]]) != 0){
      for(j in 1:length(filePath[[i]])){
        folderSize <<- folderSize + file.info(filePath[[i]][j])[,"size"]
      }
    }
  }
  print(paste("Folder size:", round(folderSize/1024/1024, 4), "MB", sep = ""))
}

# FindSameFiles -----------------------------------------------------------

FindSameFiles <- function(){
  for(i in 1:length(filePath)){
    if(length(filePath[[i]]) != 0){
      for(j in 1:length(filePath[[i]])){
        md5Index <- md5Index + 1 
        md5[md5Index] <- md5sum(filePath[[i]][j])
      }
    }
  }
  if(is.na(md5[1])){
    print("No file!")
  }else{
    dup <- unique(md5[which(duplicated(md5) == TRUE)])
    if(length(dup) == 0){
      print("No duplicated file!")
    }else{
      for(k in 1:length(dup)){
        pathIndex <- 0
        path <- list()
        for(i in 1:length(filePath)){
          if(length(filePath[[i]]) != 0){
            for(j in 1:length(filePath[[i]])){
              if(dup[k] == md5sum(filePath[[i]][j])){
                pathIndex <- pathIndex + 1
                path[[pathIndex]] <- filePath[[i]][j]
              }
            }
          }
        }
        PATH[[k]] <- path
      }
      print(paste("duplicated files' path:", PATH, sep = "\n"))
    }
  }
  print(paste("Files' quantity:", md5Index, sep = ""))
  print(paste("No-duplicated files' quantity:", length(unique(md5)), sep = ""))
}

FilePath(dirPath)
CountFolderSize()
FindSameFiles()