library(caret)
library(lubridate)



#Load in data.
traindata <- read.csv("train.csv", header = TRUE)
testdata <- read.csv("test.csv", header = TRUE)

#Reorganize data.
#traindata$Category <- make.names(traindata$Category)
traindata <- traindata[, -c(3,6)] 
traindata$Category <- as.factor(traindata$Category)
traindata$Dates <- strptime(traindata$Dates, "%Y-%m-%d %H:%M:%S", tz="GMT")
traindata$Years <- as.factor(year(traindata$Dates))
traindata$Months <- as.factor(months(traindata$Dates))
traindata$Hours <- as.factor(hour(traindata$Dates))


testdata$Dates <- strptime(testdata$Dates, "%Y-%m-%d %H:%M:%S", tz="GMT")
testdata$Years <- as.factor(year(testdata$Dates))
testdata$Months <- as.factor(months(testdata$Dates))
testdata$Hours <- as.factor(hour(testdata$Dates))


ind<- sample(2,nrow(traindata), replace = TRUE, prob = c(0.7, 0.3))
train_1<- traindata[ind==1, ]
train_2<- traindata[ind==2,]

library(randomForest)
library(doSNOW)
cl <- makeCluster(4, type="SOCK")
#registerDoSNOW(cl) #number of cores on the machine
#darkAndScaryForest <- foreach(y=seq(10), .combine=combine ) %dopar% {
rf<- randomForest(Category ~  PdDistrict + DayOfWeek  + X + Y + Years + Months + Hours, data = traindata, ntree=10,do.trace=T, importance=TRUE)
#}
rf
table(predict(rf), train2$Category)
plot(rf)
importance(rf)
iris_pred<- predict(rf, newdata = iris_test)
table(iris_pred, iris_test$species)
plot(margin(rf, iris_test$species))

library(caret)
library(lattice)
library(ggplot2)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10)
grid_rf<- expand.grid(.mtry= c(2,4,8,16))
set.seed(300)
rf<-train(Category ~  PdDistrict + Years + Months + DayOfWeek + Hours + X + Y, data=train_1, method= "rf", metric= "Kappa", trControl= ctrl, tuneGrid= grid_rf)


#set.seed(50)
#trainsmall <- traindata[sample(1:nrow(traindata), round(nrow(traindata)/50, 0)),]

#outersect <- function(x, y) {
 # sort(c(setdiff(x, y),
       #  setdiff(y, x)))
#}

##See which Category variables have not been included and add them if necessary. 
#outersect(unique(trainsmall$Category), unique(traindata$Category))

##"TREA" is missing, so add it. 
#trainsmall <- rbind(trainsmall, traindata[head(which(traindata$Category=="TREA"),1),])
#trainsmall <- trainsmall[order(trainsmall$Dates),]
#trainsmall <- droplevels(trainsmall)
#trainsmall <- trainsmall[,-5]

#Build random forst model.
#set.seed(1234)
fit <- train(Category ~ PdDistrict + Years + Months + DayOfWeek + Hours + X + Y, method="rf", data=traindata)

solution3 <- predict(rf, testdata, type="prob")
solution3 <- cbind(Id=testdata$Id, solution3)
trainnames<- names(traindata$Category)
names(solution3) <- c("Id", trainnames)

write.csv(solution3, "solution3.csv", row.names=F)
