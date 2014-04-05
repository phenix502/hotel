# 防止读入的所有string都被当成factor
options(stringsAsFactors=FALSE)

# 读入语料
Infor.pos <- readLines('material/pos2000.txt')
Infor.neg <- readLines('material/neg2000.txt')

Infor.pos <- removeEngNum(Infor.pos) 
Infor.neg <- removeEngNum(Infor.neg)

# 分词
Infor.pos <- segmentCN(Infor.pos,returnType="tm")
Infor.neg <- segmentCN(Infor.neg,returnType="tm")

# 语料库
Infor.pos <- Corpus(VectorSource(Infor.pos))
Infor.neg <- Corpus(VectorSource(Infor.neg))

#去除停止词
Infor.pos <- tm_map(Infor.pos,removeWords,stopwordsCN())
Infor.neg <- tm_map(Infor.neg,removeWords,stopwordsCN())

# 合成预料库
corpus <- c(Infor.pos, Infor.neg)

# 文本词条矩阵
corpus.dtm <- DocumentTermMatrix(corpus)



corpus.dtm <- DocumentTermMatrix(corpus,control=list(wordLengths = c(2, Inf)))

corpus.dtm <- removeSparseTerms(corpus.dtm, 0.98)




# 设置label
neg.len <- length(Infor.neg)
pos.len <- length(Infor.pos)
len <- neg.len + pos.len
corpus.df <- as.data.frame(inspect(corpus.dtm))
label <- c(rep("neg",neg.len), rep("pos",pos.len))
label <- as.factor(label)
corpus.df$label <- label


## 随机森林算法选取前100个重要的词语
weights.rf <- random.forest.importance(label~., corpus.df, 1)
subset <- cutoff.k(weights.rf, 100)


# 提取出来的特征做为一个新的数据框
d <- data.frame(word = rownames(weights.rf), freq= weights.rf$attr_importance)
# 按重要性从大到小排列
newdata <- d[order(-d$freq),]
#画出词云
wordcloud(d$word, d$freq, random.order = F, colors = brewer.pal(8, "Dark2"))


