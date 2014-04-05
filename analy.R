## 传入一个document-term matrix，使用SVM和随机森林进行分类
algorithm_summary <- function(dtm){
  neg.len <- length(Infor.neg)
  pos.len <- length(Infor.pos)
  len <- neg.len + pos.len
  
  label <- c(rep("neg",neg.len), rep("pos",pos.len))
  #从消极评论中挑选100个作为测试集，同理积极评论中挑选100个作为测试集
  neg.test <- sample(1:neg.len, 100, replace= FALSE)
  pos.test <- sample(neg.len+1:len, 100, replace= FALSE)
  testSize <- c(neg.test, pos.test)
  trainSize <- 1:len
  trainSize <- trainSize[-testSize]

  # create a container
  container.song <- create_container(dtm, label,
                                   trainSize=trainSize, testSize=testSize, virgin=FALSE)

  # training models
  SVM.song <- train_model(container.song, "SVM")
  RF.song <- train_model(container.song, "RF")

  # classifying data using trained models
  SVM_CLASSIFY.song <- classify_model(container.song, SVM.song)
  RF_CLASSIFY.song <- classify_model(container.song, RF.song)


  SVM_result <- create_precisionRecallSummary(container=container.song, classification_results=SVM_CLASSIFY.song)
  RF_result <-create_precisionRecallSummary(container=container.song, classification_results=RF_CLASSIFY.song)
  return(list(SVM_RESULT=SVM_result,RF_RESULT=RF_result))
  }
result_all_corpus <- algorithm_summary(corpus.dtm.tfidf)