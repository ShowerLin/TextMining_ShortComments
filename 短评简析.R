#shortcomment Analyze
#author = "Shower"
library(Rwordseg)
library(wordcloud2)
library(tm)
library(arules)
library(ggplot2)
library(readxl)

Results = read_excel(file.choose())
content = Results$评论内容

insertWords("阿米尔汗")
insertWords("吉朵")
insertWords("印度电影")
insertWords("励志")
installDict(file.choose(),dictname = "自定义字典.txt",dicttype = "text",load = TRUE)

segw = segmentCN(content)
stopwords = unlist(readLines(file.choose()))#读入自己的停用词表


removeStopWords <- function(x,stopwords) {
  
  temp <- character(0)
  
  index <- 1
  
  xLen <- length(x)
  
  while (index <= xLen) {
    
    if (length(stopwords[stopwords==x[index]]) <1)
      
      temp<- c(temp,x[index])
    
    index <- index +1
    
  }
  
  temp
  
}

clearedwords = lapply(segw,removeStopWords,stopwords)
tmp = lapply(clearedwords,removeStopWords,wasteword)
wordrank = as.data.frame(sort(table(unlist(tmp)),decreasing = T))
wordcloud2(wordrank[1:240,])

#关联
words_s = lapply(tmp,as.factor)
words_s = lapply(tmp,unique)
trans = as(words_s,"transactions")
items = apriori(trans, parameter = list(supp=0.01,conf=0.1,minlen = 1,target = "frequent itemsets"),control = list(verbose = F))
as(sort(items),"data.frame")
plot(items, method = "graph", control = list(type = "items", main = "短评的词汇关系，最小项集为2"))

#聚类
corpus = Corpus(VectorSource())
clearedwords.dtm = DocumentTermMatrix(corpus,control = list(wordLengths=c(1,Inf)))
clearedwords.matirx = as.matrix(clearedwords.dtm)
k = 5
kmeasnsRes = kmeans(clearedwords.matirx,5)
mode(kmeasnsRes)


