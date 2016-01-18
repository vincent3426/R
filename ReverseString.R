###############################################################################
# FileName : ReverseString.R
# Author : Sanchez Vincent
# Email : vincent3426@bupt.edu.cn
# Date : 2016-01-11 20:46:39 CST
###############################################################################

library(stringr)
ReverseString <- function(x, flag){
  tmp <- NULL
  choice = c(" ", "")
  # x in English for 1, Chinese for 2
  chars <- unlist(strsplit(x, choice[flag]))
  Cha <- chars[-which(chars == choice[-flag])] # Complementary, delete the spaces
  for(i in length(Cha):1){
    tmp <- str_trim(paste(tmp, Cha[i], sep = choice[flag]))
  }
  print(tmp)
}

x1 = "  hello my  girl, i    love you!"
x2 = "   我爱你啊, 你知道  吗?"
ReverseString(x1, 1)
ReverseString(x2, 2)
