library(dplyr)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

library(partykit)
library(caret)


library(kimisc)

## read and create data
train <- read.table("data/train.csv", header = TRUE, sep = ",")
test  <- read.table("data/test.csv", header = TRUE , sep = ",")
cross.validation <- sample.rows(subset(train),100)
train <- setdiff(train, cross.validation)


## feature engineering

#prop.table(table(train$Sex, train$Survived), margin = 1)
#prop.table(table(train$Child , train$Survived), margin = 1)

make.child <- function(train) {
    train$Child<-NA
    train$Child[train$Age<18]<-1
    train$Child[train$Age>=18]<-0
    return(train)
}


survived.factor <- function(train) {
    train$Survived <- as.factor(train$Survived)
    return(train)
}

train <- survived.factor(train)
train <- make.child(train)

cross.validation <- survived.factor(cross.validation)
cross.validation <- make.child(cross.validation)



## Generate model
rpart1 <- rpart (Survived~ Pclass + Sex + Age + SibSp +
                     Parch + Fare + Embarked , data= train , method ="class")

rpart1a <- as.party(rpart1)
rpartFull <- rpart( Survived ~ Pclass + Sex + Age + SibSp + Parch +
                       Fare + Embarked + Child, data=train, method="class")

rpartPred <- predict(rpartFull, cross.validation, type= "class")
confusionMatrix(rpartPred, cross.validation$Survived)

# tuning
cvCtrl <- trainControl(classProbs = TRUE)
# notes:
# contains na values:
#   Age, 
train$Age[is.na(train$Age)] <- 0
train$Child[is.na(train$Child)] <- 0

set.seed(1)
rpartTune <- train(fSurvived ~ Pclass + Sex + Age + SibSp + Parch +
                       Fare + Embarked + Child, data=train,
                   method = "rpart",
                   metric = "ROC",
                   trControl = cvCtrl)



#plot(fit)
#text(fit)
#fancyRpartPlot(fit)

#prediction 



prob <-predict(fit, newdata=cross.validation, type = "class")


pred.cv <- prediction(prob, cross.validation$Survived)


perf <- performance(pred.cv, measure = 'tpr', x.measure = 'fpr')
auc <- performance(pred.cv, measure='auc')
auc <- auc@y.values[[1]]


roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))


my_solution1 <-data.frame(PassengerId= test$PassengerId, Survived= my_pred)
write.csv(my_solution1, "my_solution1.csv", row.names = FALSE)


