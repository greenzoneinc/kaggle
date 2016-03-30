## Preprocessing for Titanic challenge

# load data
source("R_scripts/defineData.R")

#Embarked
table(full$Embarked)
#  C   Q   S 
#270 123 914 
# subset(full, is.na(Embarked)) -> Google -> "S"...
full$Embarked[is.na(full$Embarked)]<-'S'

#Extract Title from Name
full$Title<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
aggregate(Age~Title,full,median)
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess',
                             'Jonkheer')] <- 'Lady'

#check the result
aggregate(Age~Title,full,summary, digits=2)

#Adding FamilySize
full$FamilySize<-full$Parch+full$SibSp+1

#Fare
# create a decision tree for Fare based on 
# Pclass+Title+Sex+SibSp+Parch (1 Passenger)
fit.Fare<-rpart(Fare[!is.na(Fare)]~Pclass+Title+Sex+
                    SibSp+Parch,data=full[!is.na(full$Fare),],
                method='anova')

# display the results
printcp(fit.Fare) 
full$Fare[is.na(full$Fare)]<-predict(fit.Fare,full[is.na(full$Fare),])

#FamilyId2
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(full$FamilySize,Surname)
full$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
full$FamilyId2<-factor(FamilyId)

#Age decision tree (regression) method to predict the 20.09% missing Age data
fit.Age<-rpart(Age[!is.na(Age)]~Pclass+Title+Sex+SibSp+Parch+Fare,
               data=full[!is.na(full$Age),],method='anova')
# fancyRpartPlot(fit.Age, main="Age decision tree - predict the 20.09% missing Age data")
full$Age[is.na(full$Age)]<-predict(fit.Age,full[is.na(full$Age),])

#Adding Mother
full$Mother<-0
full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1
#Adding Child
full$Child<-0
full$Child[full$Parch>0 & full$Age<=18]<-1

#check missing 
data.frame(sapply(full,check.missing))

#Exact Deck from Cabin number
full$Deck<-sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])
deck.fit<-rpart(Deck~Pclass+Fare,data=full[!is.na(full$Deck),])
full$Deck[is.na(full$Deck)]<-as.character(predict(deck.fit,full[is.na(full$Deck),],type='class'))
full$Deck[is.na(full$Deck)]<-'UNK'

#Excat Position from Cabin number
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
levels(full$CabinPos)<-c('Front','End','Middle')
full$num<-NULL
#side.train<-full[!is.na(full$Side),]
#side.test<-full[is.na(full$Side),]
#side.fit<-rpart(Side~FamilyId+FamilySize,side.train,method='class')
#full$Side[is.na(full$Side)]<-as.character(predict(side.fit,side.test,type='class'))

#Excat Position from Cabin number
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
levels(full$CabinPos)<-c('Front','End','Middle')
full$num<-NULL
#side.train<-full[!is.na(full$Side),]
#side.test<-full[is.na(full$Side),]
#side.fit<-rpart(Side~FamilyId+FamilySize,side.train,method='class')
#full$Side[is.na(full$Side)]<-as.character(predict(side.fit,side.test,type='class'))

#factorize the categorical variables
full<-transform(full,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Child=factor(Child),
                FamilyId2=factor(FamilyId2),
                Deck=factor(Deck)
                )



