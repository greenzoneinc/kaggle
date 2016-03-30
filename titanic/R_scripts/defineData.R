## defineData.R
#
# We want to include a cross-validation set in addition to
# the training and test sets. This will give us a better idea
# of what we're missing and which corrections are available.

## read train/test data
train<-read.csv("data/train.csv", header = TRUE,
                na.strings=c('NA',''), stringsAsFactors=F)
test<-read.csv("data/test.csv", header = TRUE,
               na.strings=c('NA',''), stringsAsFactors=F)

## checking the missing data
check.missing<-function(x) return(paste0(round(sum(is.na(x))/
                                                   length(x),4)*100,'%'))
data.frame(sapply(train,check.missing))
data.frame(sapply(test,check.missing))

## define cross validation data

keep.rows <- function(sample.rows, total.rows) {
    rows <- c()
    for (rownum in 1:total.rows) {
        if (!is.element(rownum, sample.rows)) {
            rows <- c(rows, rownum)
        }
    }
    return(rows)
}

sample.rows <- sample(1:nrow(train), 100)
cross.validation <- train[sample.rows, ]
train <- train[keep.rows(sample.rows, nrow(train)), ]


#combine train/test data for pre-processing
train$Cat <- 'train'
cross.validation$Cat <- 'cross.validation'
test$Cat <- 'test'
test$Survived<-NA
full<-rbind(train,cross.validation,test)

## Change dependent variable to factor
survived.factor <- function(train) { as.factor(train$Survived) }
train$Survived<- survived.factor(train)
cross.validation$Survived <- survived.factor(cross.validation)

#combine train/test and cross validation data for pre-processing
train$Cat <-'train'
test$Cat <-'test'
cross.validation$Cat <- 'cross.validation'
test$Survived<-NA
full<-rbind(train,test, cross.validation)
