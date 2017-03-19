#### Lab 3 ####

####Best Overall Model####
################################ Boosting ( using full data set to train) ####################################

rm(list=ls())

# load data and remove null values
data<-read.csv("./Lab3Data.csv")
summary(data)
sum(is.na(data))
newdata<-na.omit(data)
newdata<-newdata[,-1]
mean(newdata$Churn!="Yes")
#73.54% of customers remained their service. 

# 0.8/0.2 split
set.seed(1)
trainindices<-sample(1:nrow(newdata),0.8*nrow(newdata))
train<-newdata[trainindices,]

#install.package ('gbm')
#install.packages("caret")
library(gbm)
library(caret)
# trainContril function allows me to control the resampling of the data. 
# Train 4 times on different portions of the data before settling on the best tuning parameters(trees, shrinkage, and interaction depth)
control <- trainControl(method='cv', number=4, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
boost.fit<-train(newdata[,1:19],newdata[,20],method="gbm",trControl=control,metric="ROC",preProc = c("center","scale"))
summary(boost.fit)
# The most important variables are contract, tenure. Internet service.
print(boost.fit)
# The last printout line shows that the tuning parameters the number of trees is 150, interation depth is 1,lambda is 0.1. 
plot(boost.fit)
pred<-predict(object=boost.fit,newdata[-trainindices,1:19])
#mytable<-table(newdata[-trainindices,20],pred)
mean(pred!=newdata[-trainindices,20])
# overall error rate is 18.72%

################################ Boosting (use 80% of data set to train)##################################
################################### (Control on some other parameters)  ####################################
rm(list=ls())

# load data and remove null values
data<-read.csv("./Lab3Data.csv")
summary(data)
sum(is.na(data))
newdata<-na.omit(data)
newdata<-newdata[,-1]
mean(newdata$Churn!="Yes")
#73.54% of customers remained their service. 

# 0.8/0.2 split
set.seed(5072)
trainindices<-sample(1:nrow(newdata),0.8*nrow(newdata))
train<-newdata[trainindices,]

library(gbm)
library(caret)

control <- trainControl(method='cv', number=4, summaryFunction = twoClassSummary,classProbs = TRUE)
grid<-expand.grid(n.trees = seq(100,1500,100), interaction.depth = 2, shrinkage = .01, n.minobsinnode = 10)
boost.fit<-train(train[,1:19],train[,20],method="gbm",trControl=control,tuneGrid=grid,metric="ROC",preProc = c("center","scale"))

summary(boost.fit)
print(boost.fit)
plot(boost.fit)
pred<-predict(object=boost.fit,newdata[-trainindices,1:19])
mean(pred!=newdata[-trainindices,20])
# overall error rate is 21.04%



####Classification tree models####
rm(list=ls())
require(rpart)
library(tree)
require(rattle)

#read in the csv for the telecom data
#we are going to build classification trees to predict churn
data<-read.csv("Lab3Data.csv",sep=",",header=T)
discardcols <- names(data) %in% c("customerID")
data<- data[!discardcols]
data<-na.omit(data)

#build the train and test
set.seed(2016)
train<-sample(1:nrow(data), size=5042)
data.test<-data[-train,]
errorRate=c()
type2=c()
type1=c()

####classification tree model####
###Building a maximal model
mymodel<-rpart(Churn~.,data=data[train,],method="class",
               parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                     maxsurrogate=0,cp=0,minbucket=1,minsplit=2))

###Pruning the maximal tree to minimize xerror
#mymodel$cptable
xerr<-mymodel$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-mymodel$cptable[minxerr,"CP"]
mymodel.prune<-prune(mymodel,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
###Predict on the test set
mymodel.prune.predict <- predict(mymodel.prune, newdata=data.test, type="class")
besttable<-table(data.test$Churn, mymodel.prune.predict,dnn=c("Actual", "Predicted"))
besterror=(besttable[2]+besttable[3])/nrow(data.test)
besttype1=besttable[3]/(besttable[1]+besttable[3])
besttype2=besttable[2]/(besttable[2]+besttable[4])
#for classification trees, this model provided the most consistent low error rate, typically around 19-20% depending on the seed

####Other Attempts####
#build the tree
tree.data=tree(Churn~.,data = data[train,])
mymodel <- rpart(Churn ~ .,data=data[train,],method="class",
                 parms=list(split="information"),control=rpart.control(minsplit=10, 
                                                                       minbucket=5,maxdepth=20,usesurrogate=0,maxsurrogate=0))

#printcp(mymodel)
plot(mymodel)
text(mymodel,pretty=0)
mymodel.predict <- predict(mymodel, newdata=data.test, type="class")
mytable<-table(data.test$Churn, mymodel.predict,dnn=c("Actual", "Predicted"))
errorRate[1]=(mytable[2]+mytable[3])/nrow(data.test)
type1[1]=mytable[3]/(mytable[1]+mytable[3])
type2[1]=mytable[2]/(mytable[2]+mytable[4])
#this model offers a slightly higher error rate than our best model

###Building a maximal model
mymodel<-rpart(Churn~.,data=data[train,],method="class",
               parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                     maxsurrogate=0,cp=0,minbucket=1,minsplit=2))
#fancyRpartPlot(mymodel, main="Maximal Decision Tree")
plot(mymodel)
#print(mymodel$cptable)
#plotcp(mymodel)
#grid()

###Predict on the test set
mymodel.predict <- predict(mymodel, newdata=data.test, type="class")
mytable2<-table(data.test$Churn, mymodel.predict,dnn=c("Actual", "Predicted"))
errorRate[2]=(mytable2[2]+mytable2[3])/nrow(data.test)
type1[2]=mytable2[3]/(mytable2[1]+mytable2[3])
type2[2]=mytable2[2]/(mytable2[2]+mytable2[4])
#this gives us a higher error rate, around 24%

###Create another model###
mymodel <- rpart(Churn ~ .,data=data[train,],method="class",
                 parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                       maxsurrogate=0,cp=0,minbucket=1,minsplit=0,xval=10))
#plot(mymodel)
#text(mymodel,pretty=0)
mymodel.predict <- predict(mymodel, newdata=data.test, type="class")
mytable<-table(data.test$Churn, mymodel.predict,dnn=c("Actual", "Predicted"))
errorRate[3]=(mytable[2]+mytable[3])/nrow(data.test)
type1[3]=mytable[3]/(mytable[1]+mytable[3])
type2[3]=mytable[2]/(mytable[2]+mytable[4])
#this one is clearly worse and has a higher error rate of 24%

####Loss Matrix####
mymodel.prune<-rpart(Churn~.,data=data[train,],method="class",
                     parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                           maxsurrogate=0,cp=0,minbucket=1,minsplit=2,loss=matrix(c(0,1,10,0), byrow=TRUE, nrow=2)))
###Still want to prune when using Loss Matrix to build maximal tree
mymodel.prune$cptable
xerr<-mymodel.prune$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-mymodel.prune$cptable[minxerr,"CP"]
mymodel.prune<-prune(mymodel.prune,cp=mincp)
###Predict on the test set
mymodel.prune.predict <- predict(mymodel.prune, newdata=data.test, type="class")
mytable=table(data.test$Churn, mymodel.prune.predict,dnn=c("Actual", "Predicted"))
errorRate[4]=(mytable[2]+mytable[3])/nrow(data.test)
type1[4]=mytable[3]/(mytable[1]+mytable[3])
type2[4]=mytable[2]/(mytable[2]+mytable[4])
#I tested with different loss matrices and received similar results
#this method gives around a 19% error rate





