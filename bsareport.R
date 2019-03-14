setwd("C:/study/Summer Internship")
HBdata<-read.csv("2008_BSA_Inpatient_Claims_PUF.csv")

library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)

HBD<-HBdata[,c(-1,-7)]
HBD<-cbind(HBD[,3],HBD[,-3])
colnames(HBD)<-c('diagnosis',"sex",'age','procedure','days','class')
HBD[,4][which(is.na(HBD[,4]))]<-100
finddiagnosis <- function(matr,k) {
  newma<-matr[which(matr[,1]==k),]
  newma<-newma[,-1]
  return(newma)
}
randchoose <- function(matr) {
  leng<-nrow(matr)
  se<-seq(1:leng)
  sam<-sample(se,as.integer(leng*0.8))
  return(sam)
}
#data visualization
#functions:
createplot <- function(matr) {
  data1<-as.data.frame(table(matr[,1]))
  colnames(data1)<-c("sex","num")
  label_value1<- paste('(', round(data1$num/sum(data1$num) * 100, 1), '%)', sep = '')
  label1 <- paste("sex ",data1$sex, label_value1, sep = '')
  pl1<-ggplot(data = data1,mapping = aes(x = 'Content', y = num, fill = sex))+geom_bar(stat = 'identity', position = 'stack',width = 0.5)+coord_polar(theta = "y")+
    labs(x="",y="",title = "")+theme(axis.text = element_blank())+theme(axis.ticks = element_blank())+ scale_fill_discrete(labels = label1)+
    geom_text(aes(y = data1$num/2+(c(0, cumsum(data1$num[length(data1$num):1])[-length(data1$num)])[length(data1$num):1]), x = 1, label = label1))
  data2<-as.data.frame(table(matr[,2]))
  colnames(data2)<-c("age","num")
  label_value2<- paste('(', round(data2$num/sum(data2$num) * 100, 1), '%)', sep = '')
  label2 <- paste("age ",data2$age, label_value2, sep = '')
  pl2<-ggplot(data = data2,mapping = aes(x = 'Content', y = num, fill = age))+geom_bar(stat = 'identity', position = 'stack',width = 0.5)+coord_polar(theta = "y")+
    labs(x="",y="",title = "")+theme(axis.text = element_blank())+theme(axis.ticks = element_blank())+ scale_fill_discrete(labels = label2)+
    geom_text(aes(y = data2$num/2+(c(0, cumsum(data2$num[length(data2$num):1])[-length(data2$num)])[length(data2$num):1]), x = 1, label = label2))
  data3<-as.data.frame(table(matr[,3]))
  colnames(data3)<-c("procedure","num")
  label_value3<- paste('(', round(data3$num/sum(data3$num) * 100, 1), '%)', sep = '')
  label3 <- paste("procedure ",data3$procedure, label_value3, sep = '')
  pl3<-ggplot(data = data3,mapping = aes(x = 'Content', y = num, fill = procedure))+geom_bar(stat = 'identity', position = 'stack',width = 0.5)+coord_polar(theta = "y")+
    labs(x="",y="",title = "")+theme(axis.text = element_blank())+theme(axis.ticks = element_blank())+ scale_fill_discrete(labels = label3)+
    geom_text(aes(y = data3$num/2+(c(0, cumsum(data3$num[length(data3$num):1])[-length(data3$num)])[length(data3$num):1]), x = 1, label = label3))
  data4<-as.data.frame(table(matr[,4]))
  colnames(data4)<-c("days","num")
  label_value4<- paste('(', round(data4$num/sum(data4$num) * 100, 1), '%)', sep = '')
  label4 <- paste("days ",data4$days, label_value4, sep = '')
  pl4<-ggplot(data = data4,mapping = aes(x = 'Content', y = num, fill = days))+geom_bar(stat = 'identity', position = 'stack',width = 0.5)+coord_polar(theta = "y")+
    labs(x="",y="",title = "")+theme(axis.text = element_blank())+theme(axis.ticks = element_blank())+ scale_fill_discrete(labels = label4)+
    geom_text(aes(y = data4$num/2+(c(0, cumsum(data4$num[length(data4$num):1])[-length(data4$num)])[length(data4$num):1]), x = 1, label = label4))
  list(pl1,pl2,pl3,pl4) 
}

#example one
exem1<-finddiagnosis(HBD,20)
set.seed(39)
train1<-exem1[c(randchoose(exem1)),]
set.seed(39)
test1<-exem1[-c(randchoose(exem1)),]

tree1<-rpart(class ~ ., data=train1, method="class",parms=list(split="information"))
tree1$cptable
plotcp(tree1)
prunedtree1<-prune(tree1,cp=0.01000000)
prp(prunedtree1, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")
ptree1<-predict(prunedtree1,test1,type = "class")
table(test1[,5], ptree1,dnn=c("Actual", "Predicted"))
length(which(as.numeric(ptree1)==as.numeric(test1[,5])))/length(ptree1)
length(which(abs(as.numeric(ptree1)-as.numeric(test1[,5]))<=1))/length(ptree1)

forest1<-randomForest(as.factor(class)~.,data = train1,importance=TRUE)
pforest1<-predict(forest1,test1)
forest1 
table(test1[,5], pforest1,dnn=c("Actual", "Predicted"))
#cbind(pforest1,test1[,5])
length(which(as.numeric(pforest1)==as.numeric(test1[,5])))/length(pforest1)
length(which(abs(as.numeric(pforest1)-as.numeric(test1[,5]))<=1))/length(pforest1)

createplot(exem1)

#example two
exem1<-finddiagnosis(HBD,80)
set.seed(39)
train1<-exem1[c(randchoose(exem1)),]
set.seed(39)
test1<-exem1[-c(randchoose(exem1)),]

tree1<-rpart(class ~ ., data=train1, method="class",parms=list(split="information"))
tree1$cptable
plotcp(tree1)
prunedtree1<-prune(tree1,cp=0.01000000)
prp(prunedtree1, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")
ptree1<-predict(prunedtree1,test1,type = "class")
table(test1[,5], ptree1,dnn=c("Actual", "Predicted"))
length(which(as.numeric(ptree1)==as.numeric(test1[,5])))/length(ptree1)
length(which(abs(as.numeric(ptree1)-as.numeric(test1[,5]))<=1))/length(ptree1)

forest1<-randomForest(as.factor(class)~.,data = train1,importance=TRUE)
pforest1<-predict(forest1,test1)
forest1 
table(test1[,5], pforest1,dnn=c("Actual", "Predicted"))
#cbind(pforest1,test1[,5])
length(which(as.numeric(pforest1)==as.numeric(test1[,5])))/length(pforest1)
length(which(abs(as.numeric(pforest1)-as.numeric(test1[,5]))<=1))/length(pforest1)

createplot(exem1)
