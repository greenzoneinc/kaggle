library(MASS)
library(readr)
#library (rpart)
library(caret)
library(lubridate)

explore<-function(x){
  barplot(table(x),horiz=T,cex.names=0.7,las=2)
}

treatment<-function(fname){
  df<-read.csv(paste0('../data/',fname))
  #df<-read_csv(paste0('../data/',fname))
  Dates1  = strptime(as.character(df$Dates),"%Y-%m-%d %H:%M:%S")
  print(str(Dates1))
  df$Year = Dates1$year
  df$Month = Dates1$mon
  df$Hour = as.numeric(format(ymd_hms(Dates1), "%H"))
  #df$Loc = paste0('(',round(df$X,2),',',round(df$Y,2),')')
  df$Loc = as.factor(paste(round(df$X,2), round(df$Y,2), sep= " "))
  
  return(df)
}
train<-treatment('train.csv')
print(str(train))
print('Read train data complete...')
test<-treatment('test.csv')
print('Read test data complete...')
#District and Locations are explored with significnat impact on type of crime
#explore(train$PdDistrict)
#explore(train$Loc)

##split train data into 10 paritions due to memory space constraint
inTrain<-createDataPartition(train$Category,p=0.1,list=F)
train.sub<-train[inTrain,]
#rm(train)
## create raprt training model
rpart.train<-function(train,test){
  submission<-data.frame(Id=test$Id)
  response<-data.frame(Cat=train$Category)
  #extract the names of crime
  crime<-as.character(unique(train$Category))
  crime<-sort(crime)
  for (i in crime){
    #i = 'ASSAULT'
    response[i]<- 0
    response[i][response$Cat==i,]<- 1
    fit<-glm(response[,i]~PdDistrict+X:Y+DayOfWeek+Year+Hour+Month ,data=train, family = binomial)
    pred <- predict(fit,test, type = "response")
    submission[i]<-pred
    print(paste0(ncol(submission)/length(crime)*100,'% completed'))
  }
  return(submission)
}
submission<-rpart.train(train.sub,test)
rm(train, test,train.sub)
write.csv(submission,'submission.csv',row.names=F)
gz_out <- gzfile("submit.csv.gz", "w")
writeChar(write_csv(submission, ""), gz_out, eos=NULL)
close(gz_out)