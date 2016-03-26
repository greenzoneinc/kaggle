## Replicating and extending: 
## https://www.kaggle.com/skobnikoff/titanic/randomforest-cforest-method

#loading libraries
library(randomForest)
library(party)
library(rpart)

source("R_scripts/preprocess.R")

#split train/test data
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)


#cforest (conditional inference tree) method, (support variables with more levels and missing values, with unbiased prediction)
fit.cf<-cforest(Survived~FamilyId2+CabinPos+Deck+Pclass+Sex+Age+
                    SibSp+Parch+Fare+Embarked+Title+Mother+Child+
                        Deck,data=train,
                controls=cforest_unbiased(ntree=500, mtry=3))

#write submission
test$Survived<-predict(fit.cf,test,OOB=TRUE,type='response')
submission<-test[,1:2]
write.csv(submission,'submissions/submission_cforest2.csv',row.names=F)
