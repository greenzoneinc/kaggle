# This script creates and evaluates a random forest model in R for the 'San Francisco Crime Classification' competition.
# This iteration is only to get the model up and running, so there is minimal feature engineering,
# parameter tuning, and visualization.

# Load packages.
library(data.table)
library(randomForest)

# Import data.
train = data.table(read.csv('train.csv',header = T))
test = data.table(read.csv('test.csv', header = T))

#####
# PRE-PROCESSING AND FEATURE ENGINEERING.

# Rename columns of testing and training data set.
setnames(train, names(train), c('date', 'category_predict', 'description_ignore', 'day_of_week', 'pd_district', 'resolution', 'address', 'x', 'y'))
setnames(test, names(test), c('id', 'date', 'day_of_week', 'pd_district', 'address', 'x', 'y'))

# Get hour of each crime.
train$hour = as.numeric(substr(train$date, 12, 13))
test$hour = as.numeric(substr(test$date, 12, 13))

#####
# CREATE MODEL AND PREDICTIONS.

# Define model.
model = category_predict ~ day_of_week + pd_district + x + y + hour

# Set seed for reproducibility.
set.seed(1)

# Create random forest.
rf = randomForest(model, data = train, ntree = 10, importance = T)

# View feature importance.
varImpPlot(rf)
table(predict(rf), train$category_predict)
plot(rf)
importance(rf)
test_pred<- predict(rf, newdata = test)
table(test_pred, test$Category)
plot(margin(rf, iris_test$species))
#tune model 

library(caret)
library(lattice)
library(ggplot2)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10)
grid_rf<- expand.grid(.mtry= c(2,4,6,8))
set.seed(300)
m_rf<-train(model, data=train, method= "rf", metric= "Kappa", trControl= ctrl, tuneGrid= grid_rf)

rf = randomForest(model, data = train, ntree = 10, importance = T)
# Generate predictions for training and test data.
# For training data, I want accuracy.
# For test data, the submission format allows probabilities for each crime type for each crime (instead of 1 category as the final prediction).
# These different needs inform the different 'type' of predictions.
train_pred = data.table(predict(rf,  newdata = train,  type = 'response'))
test_pred = data.table(predict(rf,  newdata = test,  type = 'prob'))

#####
# CHECK TRAINING SET ACCURACY.
# I'll check OOB (out of bag) error estimate and model performance on the training data set.

# OOB error is near the top of the following output.
# For random forests, OOB error makes cross validation unnecessary.
rf

# Compute model performance on training data.

# Add training set predictions to 'train'.
train$pred = train_pred$V1

# View training accuracy.
print('Training Set Accuracy')
table(train$category_predict == train$pred)
prop.table(table(train$category_predict == train$pred))

#####
# EXPORT TEST SET PREDICTIONS.

# Predictions must be formatted as specified on Kaggle.com.
# This formatting is done for test data only.

# Add 'test$id' to 'test_pred', which just counts rows.
test_pred = cbind(test$id, test_pred)

# Rename columns.
setnames(test_pred, names(test_pred), c('Id', names(test_pred)[2:ncol(test_pred)]))

# Create csv file of test predictions.
# This is commented out for now, since I don't actually want to create a csv.
write.csv(test_pred, 'test_pred_benchmark_rf.csv', row.names = F)
