###############################################################################
# FileName : Crawler.R
# Author : Sanchez Vincent
# Email : vincent3426@bupt.edu.cn
# Begin Time : 2016-01-13 21:30:36 CST
# End Time : 2016-01-18 21:34:22 CST
# Notice : 
# 1.百度糯米_重庆渝北区宾馆团购
# 2.Write into Mysql(For learning)
###############################################################################

rm(list = ls())

packages <- c("XML", "plyr", "RCurl", "RMySQL")
uninstalledPackages <- setdiff(packages, rownames(installed.packages()))
if (length(uninstalledPackages) > 0) {
  install.packages(uninstalledPackages)  
}
beginTime <- Sys.time()
library(XML)
library(plyr)
library(RCurl)
library(RMySQL)
# 百度糯米酒店-重庆市渝北区
urls <- paste("http://t.nuomi.com/3/cq-3402-0-0-642_934_935_936_937_938_939_940_941_942_943_944_945_946/0-page", 
              seq(23), "?#j-sort-bar", sep = "")
# Request headers (Subscribe in Chrome-F12-Network)
myHttpHeader <- c("User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2540.0 Safari/537.36", 
              "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8", 
              "Accept-Language" = "zh-CN,zh;q=0.8,en;q=0.6,zh-TW;q=0.4", 
              "Connection" = "keep-alive")
cHandle <- getCurlHandle(httpheader = myHttpHeader)
d <- debugGatherer()

# names(d$value())
# cat(d$value()[3])

hotelTitle<-c("")
hotelPosition <- c("")
originalPrice<-c("")
currentPrice<-c("")
hotelSold <- c("")
hotelScores <- c("")

for(url in urls){
  webpage <- getURL(url, .opts = list(debugfunction = d$update, verbose = TRUE), curl = cHandle, .encoding="utf-8")
  # fix Chinese encoding error
  pagexml <- xmlRoot(htmlTreeParse(webpage, encoding = "utf-8", useInternalNodes = TRUE))
  originalPrice <- c(originalPrice, xpathSApply(pagexml, "//*/span [@class='ori-price']", xmlValue))
  currentPrice<-c(currentPrice, xpathSApply(pagexml, "//*/span [@class='price']", xmlValue))
  hotelSold <- c(hotelSold, xpathSApply(pagexml, "//*/span [@class='sold']", xmlValue))
  hotelScores<-c(hotelScores, xpathSApply(pagexml, "//*/span [@class='comment'] | //*/span [@class='comment-disable']", xmlValue))
  hotelTitle <- c(hotelTitle, xpathSApply(pagexml, "//*/h4 [@class='title']", xmlValue))
  hotelPosition <- c(hotelPosition, xpathSApply(pagexml, "//*/div [@class='range-inner']", xmlValue))
  Sys.sleep(1)
}

# string process-delete character
# hotelPosition <- gsub('^ ', "", hotelPosition) # 无法去除类似空格的字符
hotelPosition <- substr(hotelPosition, 2, nchar(hotelPosition))
originalPrice <- gsub(substr(originalPrice, 1, 3)[2], "", originalPrice)
currentPrice <- gsub(substr(currentPrice, 1, 1)[2], "", currentPrice)
hotelSold <- gsub("已售", "", hotelSold)
hotelSold <- gsub("新单", 0, hotelSold)
hotelScores[which(hotelScores == "暂无评分")] <- NA
hotelScores <- gsub("分", "", hotelScores)

content <- data.frame(hotelTitle, hotelPosition, originalPrice, currentPrice, hotelSold, hotelScores)[-1,]
row.names(content) <- seq(dim(content)[1])
save(content, file = "百度糯米_重庆渝北区宾馆团购.RData")
write.csv(content, file = "百度糯米_重庆渝北区宾馆团购.csv")
endTime <- Sys.time()
duration <- endTime - beginTime

conn <- dbConnect(MySQL(), dbname = "crawler", username = "root", password = "***", host = "127.0.0.1", port = 3306)
if(dbExistsTable(conn,'hotel_cq_yubei_baidunuomi')){
  dbRemoveTable(conn, "hotel_cq_yubei_baidunuomi")
}
dbSendQuery(conn, 'SET NAMES utf8') # store the Chinese character 
dbSendQuery(conn, 'CREATE TABLE hotel_cq_yubei_baidunuomi(hotelTitle varchar(255), hotelPosition varchar(255), 
            originalPrice int, currentPrice int, hotelSold int, hotelScores char(4))')
# dbWriteTable(conn, "hotel_cq_yubei_baidunuomi", content) # no capitalize, can't display the chinese character
strSQL <- paste('INSERT INTO hotel_cq_yubei_baidunuomi values', 
                paste(sprintf("('%s', '%s', %i, %i, %i, '%s')", as.character(content$hotelTitle), 
                              as.character(content$hotelPosition), as.numeric(as.character(content$originalPrice)), 
                              as.numeric(as.character(content$currentPrice)), as.numeric(as.character(content$hotelSold)), 
                              as.numeric(as.character(content$hotelScores))), collapse=', '), sep = "")
dbSendQuery(conn, strSQL)
dbSendQuery(conn, 'SET NAMES gbk') # display the Chinese character
head(dbReadTable(conn, "hotel_cq_yubei_baidunuomi"))
dbDisconnect(conn)
