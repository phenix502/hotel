## 由两个character类型的变量
## 形成语料库
removeEngNum <- function(x){
  x <- gsub("[a-z]+|[A-Z]+","",x)
  x <- gsub("[0-9]+","",x)
  x <- x[x!=""]
}

