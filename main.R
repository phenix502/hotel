#加载包
# Fselector，用随机森林算法选择最重要的前几个变量
# 此文档为重要分析文档，会调用别的函数
library(tm)
library(Rwordseg)
library(RTextTools)
library(FSelector)
library(tmcn)
library(randomForest)
library(wordcloud)




# 防止读入的所有string都被当成factor
options(stringsAsFactors=FALSE)

# 读入语料
Infor.pos <- readLines('material/pos2000.txt')
Infor.neg <- readLines('material/neg2000.txt')
## 第一个乱码，去掉了，第二条和第三条评论相同去掉
Infor.pos<-Infor.pos[-886]


Infor.neg <- Infor.neg[3:length(Infor.neg)]

# 分词并形成语料库
corpus <- makeCorpus(Infor.pos, Infor.neg)

# 形成DocumentTermMatrix
corpus.dtm.tfidf <- dtm(corpus, tfidf=TRUE)
# 新改的
corpus.dtm.tfidf<- DocumentTermMatrix(corpus)






# 转为data frame
corpus.df <- as.data.frame(inspect(corpus.dtm.tfidf))

neg.len <- length(Infor.neg)
pos.len <- length(Infor.pos)
len <- neg.len + pos.len

corpus.df$label <- c(rep("neg",neg.len), rep("pos",pos.len))




## 随机森林算法选取前100个重要的词语
weights.rf <- random.forest.importance(label~., corpus.df, 1)
subset <- cutoff.k(weights.rf, 100)


## 把提取的特征作为新的docment-term matrix
feature.df <- as.DocumentTermMatrix(corpus.df[subset],weighting=weightTf)


result_all_corpus <- algorithm_summary(corpus.dtm.tfidf)
result_feature <- algorithm_summary(feature.df)




# 提取出来的特征做为一个新的数据框
d <- data.frame(word = rownames(weights.rf), freq= weights.rf$attr_importance)
# 按重要性从大到小排列
newdata <- d[order(-d$freq),]
#画出词云
wordcloud(d$word, d$freq, random.order = F, colors = brewer.pal(8, "Dark2"))

