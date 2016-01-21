###############################################################################
# FileName : amazonCrawler.R
# Author : Sanchez Vincent
# Email : vincent3426@bupt.edu.cn
# Begin Time : 2016-01-21 18:31:34 CST
# End Time : 2016-01-21 21:52:16 CST
# Notice : 
# 1.亚马逊百大畅销书排行榜——Kindle电子书销售排行榜——付费排行榜&免费排行榜
# 2.Crawling date ：2016-01-21
# 3.Subscribe the webpage found that they use AJAX, so the return results are 
#   completely the same with the first page.(Asynchronous JavaScript and XML)
# 4.We can get the ajaxurl-href in the first page with the help of Chrome F12.
#   ajaxurl = "&ajax=1" + href
###############################################################################

rm(list = ls())

library(XML)

amazonPhraseXml <- function(rootNode, flag){
  # paid list (the over items are AD)
  # rank
  paidListRank <- as.numeric(gsub("\\.", "", xpathSApply(rootNode,"//div[@class='zg_rankLine']", xmlValue)))[order[[flag]]]
  # titles
  paidListTitles <- xpathSApply(rootNode,"//div[@class='zg_title']", xmlValue)[order[[flag]]]
  # authors
  paidListAuthors <- gsub("\n\n\n\n\n\n\n~ ", "", xpathSApply(rootNode,"//div[@class='zg_byline']", xmlValue))[order[[flag]]]
  # price
  paidListPrice <- switch(flag, as.numeric(unlist(strsplit(xpathSApply(rootNode, "//strong[@class='price']", xmlValue)
                                                           [c(T, F)], " "))[c(F,T)])[1:20], rep(0, 20))
  # stars
  paidListRate <- as.numeric(substr(xpathSApply(rootNode,"//span[@class='a-icon-alt']", xmlValue), 3, 5))[order[[flag]]]
  # commentsNum
  paidListCommentsNum <- as.numeric(gsub(",", "", xpathSApply(rootNode,"//a[@class='a-link-normal']", xmlValue)))[order[[flag]]]
  return (data.frame(paidListRank, paidListTitles, paidListAuthors, paidListPrice, paidListRate, paidListCommentsNum))
}

# URLs <- paste0("http://www.amazon.cn/gp/bestsellers/digital-text/116169071/ref=sa_menu_kindle_l3_116169071#", 1:5)
URLs <- paste0("http://www.amazon.cn/gp/bestsellers/digital-text/116169071/ref=zg_bs_116169071_pg_", 1:5, 
               "/479-7055096-1052846?ie=UTF8&pg=", 1:5)
order <- list()
order[[1]] <- c(T, F)
order[[2]] <- c(F, T)
for(i in 1:length(URLs)){
  rootNode <- xmlRoot(htmlParse(URLs[i], encoding = "UTF-8"))
  if(i == 1){
    amazonBestSellerPaidList <- amazonPhraseXml(rootNode, 1)
    amazonBestSellerFreeList <- amazonPhraseXml(rootNode, 2)
  }else{
    # Sys.sleep(runif(1, 1, 2))
    amazonBestSellerPaidList <- rbind(amazonBestSellerPaidList, amazonPhraseXml(rootNode, 1))
    amazonBestSellerFreeList <- rbind(amazonBestSellerFreeList, amazonPhraseXml(rootNode, 2))
  }
}