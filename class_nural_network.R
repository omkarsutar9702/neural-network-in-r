setwd("D:/R files/R_file/class work")
#import library
library(neuralnet)
library(tidyquant)
library(caret)
#import dataset
df <- read.csv("D:/R files/R_file/heart EDA/heart.csv")

#create variable index
#random samplling
samplesize <- 0.80*nrow(df)
set.seed(8)
index <- sample(seq_len(nrow(df)) , size = samplesize)

#create training and testing data
train<-df[index,]
test<-df[-index,]

#scale the data
max<-apply(df , 2 , max)
min<-apply(df ,2 , min )
scaled<- as.data.frame(scale(df , center = min ,scale=max-min ))


#fit the neural network
tarin_nn = scaled[index,]
test_nn = scaled[-index,]


#fir the neural network
set.seed(20)
neural_network=neuralnet(target~. ,tarin_nn, hidden = 5 , linear.output = TRUE)
plot(neural_network)

#prediction using neural network
predict_test<-compute(neural_network , test_nn[,c(1:14)])
predict_test<-(predict_test$net.result * (max(df$target)-min(df$target)))+min(df$target)

plot(test_nn$target, predict_test , col="red" ,pch=16)
abline(0,1)
RMSE.NN<- (sum((test_nn - predict_test)^2)/nrow(test_nn))^0.5
RMSE.NN


#test the result
results<-compute(neural_network , test_nn)
pred<-neural_network$net.result
actual<-test$target
results_nr<-data.frame(acutal=test_nn$target , pred )
head(results_nr)

#confusion matrix
roundedreuslut<-sapply(results_nr , round,digits=0)
roundedreuslutdf<-data.frame(roundedreuslut)
attach(roundedreuslutdf)
table(acutal ,structure.c.0.906196458594508...0.0313621998647011..0.907241911195394..)

