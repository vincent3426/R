###############################################################################
# FileName : FileRename.R
# Author : Sanchez Vincent
# Email : vincent3426@bupt.edu.cn
# Date : 2016-01-13
# Notice : 
# 1.The directory can only include the pure files without directory.
# 2.It renames the file names to increasing numbers(1,2,3……) and retains the various suffix.
###############################################################################

library(tools)
rm(list = ls())

dirPath <- choose.dir()
cnt <<- 0
tmp <<- list()
filePath <<- list()

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

# FileRename -------------------------------------------------------

### arg0 pass the path of files
FileRename <- function(arg0){
  lf <- arg0
  # classify with suffix
  suffixSet <- array()
  for(i in 1:length(lf)){
    suffixSet[i] <- unlist(strsplit(lf[i], "\\."))[2]
  }
  suffix <- dimnames(table(suffixSet))$suffixSet
  for(i in 1:length(suffix)){
    index <- grep(suffix[i], lf)
    for(j in 1:length(index)){
      file.rename(lf[index[j]], paste(substr(lf[i], 1, unlist(gregexpr("\\\\",lf[i]))[2]),
                                      j, ".", suffix[i], sep=""))
    }
  }
  print("Finished rename!Check your directory...")
}

FilePath(dirPath)
FileRename(filePath[[1]])
