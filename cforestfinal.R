library(randomForest)
library(party)
library(rpart)
library(rattle)

#read train/test data
train<-read.csv("/Users/suoliyun/Documents/train.csv",na.strings=c('NA',''),stringsAsFactors=F)
test<-read.csv("/Users/suoliyun/Documents/test.csv",na.strings=c('NA',''),stringsAsFactors=F)

#combine train/test data for pre-processing
train$Cat<-'train'
test$Cat<-'test'
test$Survived<-NA
full<-rbind(train,test)

#Checking the missing data
check.missing<-function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(train,check.missing))
data.frame(sapply(test,check.missing))
data.frame(sapply(full,check.missing))

#Full fill missing Embarked with the most popular one /table(full$Embarked)
full$Embarked[is.na(full$Embarked)]<-'S'

#Adding Title
full$Title<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
aggregate(Age~Title,full,median)
full$Title[full$Title %in% c('Mme', 'Mr')] <- 'Mr'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
full$Title[full$Title %in% c('Ms','Mlle')] <- 'Miss'
#check the result
#aggregate(Age~Title,full,summary, digits=2)

#Adding FamilyID
full$FamilySize<-full$Parch+full$SibSp+1
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
full$FamilyID <- paste(as.character(full$FamilySize), full$Surname, sep="")
full$FamilyID[full$FamilySize <= 2] <- 'Small'
full$FamilyID<-factor(FamilyId)


#Adding Fare with regression
# create a decision tree for Fare based on Pclass+Title+Sex+SibSp+Parch (1 Passenger)
fit.Fare<-rpart(Fare[!is.na(Fare)]~Pclass+Title+Sex+SibSp+Parch,data=full[!is.na(full$Fare),],method='anova')
#display Fare tree
#printcp(fit.Fare) 
#fancyRpartPlot(fit.Fare, main="Fare decision tree - overkill, predicting the 1 single missing Fare")
full$Fare[is.na(full$Fare)]<-predict(fit.Fare,full[is.na(full$Fare),])

#Adding Age with regression
fit.Age<-rpart(Age[!is.na(Age)]~Pclass+Title+Sex+SibSp+Parch+Fare,data=full[!is.na(full$Age),],method='anova')
#display Age tree
fancyRpartPlot(fit.Age, main="Age decision tree - predict the 20.09% missing Age data")
full$Age[is.na(full$Age)]<-predict(fit.Age,full[is.na(full$Age),])

#Adding Mother
full$Mother<-0
full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1

#Adding Child
full$Child<-0
full$Child[full$Parch>0 & full$Age<=18]<-1

#check missing 
data.frame(sapply(full,check.missing))

#Adding Deck
full$Deck<-sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])
deck.fit<-rpart(Deck~Pclass+Fare,data=full[!is.na(full$Deck),])
full$Deck[is.na(full$Deck)]<-as.character(predict(deck.fit,full[is.na(full$Deck),],type='class'))
full$Deck[is.na(full$Deck)]<-'UNK'

#Adding CabinPos
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
levels(full$CabinPos)<-c('Front','End','Middle')
full$num<-NULL

#factorize the categorical variables
full<-transform(full,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Child=factor(Child),
                FamilyID=factor(FamilyID),
                Deck=factor(Deck)
)

#split train/test data
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)

#randomForest method, (not support variables with too many levels, e.g. FamilyId here)
fit.rf<-randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child+Deck,data=train,ntree=1250,importance=T)
plot(fit.rf,main='randomForest error rate')
imp<-importance(fit.rf,type='1')
imp<-imp[order(imp),]
(imp)
varImpPlot(fit.rf, main="randomForest - variable Importance")

#write submission
test$Survived<-predict(fit.rf,test,OOB=TRUE,type='response')
submission<-test[,1:2]
write.csv(submission,'submission_randomForest.csv',row.names=F)

#cforest (conditional inference tree) method, (support variables with more levels and missing values, with unbiased prediction)
fit.cforest<-cforest(Survived~FamilyID + CabinPos + Deck + Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Mother + Child,data=train,controls=cforest_unbiased(ntree=500, mtry=3))

#write submission
test$Survived<-predict(fit.cf,test,OOB=TRUE,type='response')
submission<-test[,1:2]
write.csv(submission,'submission_cforest.csv',row.names=F)
