library(dplyr)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.table("train.csv", header = TRUE, sep = ",")
test  <- read.table("test.csv", header = TRUE , sep = ",")

prop.table(table(train$Sex, train$Survived), margin = 1)
train$Child<-NA
train$Child[train$Age<18]<-1
train$Child[train$Age>=18]<-0
prop.table(table(train$Child , train$Survived), margin = 1)

##Exploratory Data Analysis

desc_tree<- rpart (Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked , data= train , method ="class")
plot(desc_tree)
text(desc_tree)
fancyRpartPlot(desc_tree)

#prediction 

my_pred <-predict(desc_tree, test, type = "class")
my_solution1 <-data.frame(PassengerId= test$PassengerId, Survived= my_pred)
write.csv(my_solution1, "my_solution1.csv", row.names = FALSE)


