library(caret)
library(lubridate)

#Load in data.
traindata <- read.csv("train.csv")
testdata <- read.csv("test.csv")

#Store names of Category variables. This will come in handy later!
trainnames <- sort(unique(as.character(traindata$Category)))

#Reorganize data.
traindata$Category <- make.names(traindata$Category)
traindata$Category <- as.factor(traindata$Category)
traindata$Dates <- strptime(traindata$Dates, "%Y-%m-%d %H:%M:%S", tz="GMT")
traindata$Years <- as.factor(year(traindata$Dates))
traindata$Months <- as.factor(months(traindata$Dates))
traindata$Hours <- as.factor(hour(traindata$Dates))
traindata <- traindata[, -c(3,6)]

testdata$Dates <- strptime(testdata$Dates, "%Y-%m-%d %H:%M:%S", tz="GMT")
testdata$Years <- as.factor(year(testdata$Dates))
testdata$Months <- as.factor(months(testdata$Dates))
testdata$Hours <- as.factor(hour(testdata$Dates))

#Build lda model.  
set.seed(1234)
Sys.time()
fit <- train(Category ~ PdDistrict + Years + Months + DayOfWeek + Hours + X + Y, data=traindata, method="lda")
Sys.time()

#Build solution from LDA fit.
solution <- predict(fit, testdata, type="prob")
solution <- cbind(Id=testdata$Id, solution)

names(solution) <- c("Id", trainnames)

write.csv(solution, "solution.csv", row.names=F)
