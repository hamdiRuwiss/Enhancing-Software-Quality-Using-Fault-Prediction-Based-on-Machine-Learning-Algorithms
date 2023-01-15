library(caret)
library(tidyverse)
library(pROC)

err_metric=function(CM)
{
TN =CM[1,1]
TP =CM[2,2]
FP =CM[1,2]
FN =CM[2,1]
precision =(TP)/(TP+FP)
recall_score =(TP)/(TP+FN)
f1_score=2*((precision*recall_score)/(precision+recall_score))
accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
False_positive_rate =(FP)/(FP+TN)
False_negative_rate =(FN)/(FN+TP)
print(paste("Precision value of the model: ",round(precision,4)*100))
print(paste("Accuracy of the model: ",round(accuracy_model,4)*100))
print(paste("Recall value of the model: ",round(recall_score,4)*100))
print(paste("False Positive rate of the model: ",round(False_positive_rate,4)*100))
print(paste("False Negative rate of the model: ",round(False_negative_rate,4)*100))
print(paste("f1 score of the model: ",round(f1_score,4)*100))
}


data <- read.csv('data/github/expr2/Netty-Unified.csv')
dim(data)
glimpse(data)
data = data[,-1]
dim(data)
glimpse(data)

data$bug = as.factor(data$bug)

parts = createDataPartition(data$bug, p = .7, list = F)
train = data[parts, ]
test = data[-parts, ]


grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
train_control = trainControl(method = "cv", number = 5, search = "grid")
mdl <- train(bug~.,data=train,tuneGrid=grid,trControl=train_control,method="C5.0")
logit_P = predict(mdl , newdata = test[-11] )
cm=table(test[,11],logit_P)
print(cm)
err_metric(cm)


logit_P = predict(mdl , newdata = test[-11] ,type = 'prob' )
roc_score=roc(test[,11],logit_P[,2])
roc_score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")
