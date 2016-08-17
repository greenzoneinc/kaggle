
library(data.table)
library(xgboost)
library(caret)

train <- read.table("train.csv", header = TRUE, sep = ",")
complete_train <- train[complete.cases(train), ]
test <- read.table("test.csv", header = TRUE, sep = ",")
data <- merge(train, test, by=c('Dates', 'DayOfWeek', 'Address', 'X', 'Y', 'PdDistrict'), all=TRUE)
#odata<- tbl_df(data)
n <- nrow(data)
str(data)
# parsing date
date_and_time <- strptime(data$Dates, '%Y-%m-%d %H:%M:%S')
data$Year <- as.numeric(format(date_and_time, '%Y'))
data$Month <- as.numeric(format(date_and_time, '%m'))
data$Day <- as.numeric(format(date_and_time, '%d'))
data$Week <- as.numeric(format(date_and_time, '%W'))
data$Hour <- as.numeric(format(date_and_time, '%H'))
#data$Minute <- as.numeric(format(date_and_time, '%M'))
colnames(data)

#library(plyr)
#library(dplyr)
#data<- data %>% select(c("Dates","DayofWeek", "Address", "X", "Y", "PdDistrict", "Category", "Id","Year", "Month", "Day", "Week", "Hour"))
#data<- data %>% select(-Descript, -Resolution)
# removing not-necessary columns
columns <- c('Descript', 'Resolution')
for (column in columns){
  data[[column]] <- NULL
}

# feature engineering
data$AddressTypeIsOf <- rep(FALSE, n)
data$AddressTypeIsOf[grep('.?of.?', data$Address)] <- TRUE


#separate train and test data again
idx <- data[which(!is.na(data$Category))]
classes <- sort(unique(data[idx]$Category))
m <- length(classes)
data$Class <- as.integer(factor(data$Category, levels=classes)) - 1

dim(data)

ind<- sample(2,nrow(complete_train), replace = TRUE, prob = c(0.7, 0.3))
train_1<- complete_train[ind==1, ]
train_2<- complete_train[ind==2,]

library(randomForest)
rf<- randomForest(Category ~ ., data = train_1, ntree=100, proximity=TRUE)
rf
table(predict(rf), iris_train$species)
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
m_rf<-train(default~ ., data=credit, method= "rf", metric= "Kappa", trControl= ctrl, tuneGrid= grid_rf)


# X-Y plane rotation using PCA
idx <- with(data, which(Y == 90))
transform <- preProcess(data[-idx, c('X', 'Y'), with=FALSE], method = c('center', 'scale', 'pca'))
pc <- predict(transform, data[, c('X', 'Y'), with=FALSE]) 
data$X <- pc$PC1
data$Y <- pc$PC2
# time features
data$MinuteAbs30 <- abs(as.numeric(format(date_and_time, '%M')) - 30)
#data$Minute <- NULL

# test/train separation
idx <- which(!is.na(data$Category))
classes <- sort(unique(data[idx]$Category))
m <- length(classes)
data$Class <- as.integer(factor(data$Category, levels=classes)) - 1
dim(data)

feature.names <- names(data)[which(!(names(data) %in% c('Id', 'Address', 'Dates', 'Category', 'Class')))]
for (feature in feature.names){
  if (class(data[[feature]]) == 'character'){
    cat(feature, 'converted\n')
    levels <- unique(data[[feature]])
    data[[feature]] <- as.integer(factor(data[[feature]], levels=levels))
  }
}

param <- list(
  #nthread             = 4,
  booster             = 'gbtree',
  objective           = 'multi:softprob',
  num_class           = m,
  eta                 = 1.0,
  #gamma               = 0,
  max_depth           = 6,
  #min_child_weigth    = 1,
  max_delta_step      = 1
  #subsample           = 1,
  #colsample_bytree    = 1,
  #early.stop.round    = 5
)

h <- sample(1:length(idx), floor(9*length(idx)/10))
dval <- xgb.DMatrix(data=data.matrix(data[idx[-h], feature.names, with=FALSE]), label=data[idx[-h]]$Class)
dtrain <- xgb.DMatrix(data=data.matrix(data[idx[h], feature.names, with=FALSE]), label=data[idx[h]]$Class)
watchlist <- list(val=dval, train=dtrain)
bst <- xgb.train( params            = param,
                  data              = dtrain,
                  watchlist         = watchlist,
                  verbose           = 1,
                  eval_metric       = 'mlogloss',
                  nrounds           = 15
)

# making predictions
dtest <- xgb.DMatrix(data=data.matrix(data[-idx,][order(Id)][,feature.names, with=FALSE]))
prediction <- predict(bst, dtest)
prediction <- sprintf('%f', prediction)
prediction <- cbind(data[-idx][order(Id)]$Id, t(matrix(prediction, nrow=m)))
dim(prediction)

colnames(prediction) <- c('Id', classes)
#names(prediction)
write.csv(prediction, 'submission.csv', row.names=FALSE, quote=FALSE)
#zip('submission.zip', 'submission.csv')