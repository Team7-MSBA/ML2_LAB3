#### Lab 3 ####
#Classification tree models
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

####best model####
###Building a maximal model
mymodel<-rpart(Churn~.,data=data[train,],method="class",
               parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                     maxsurrogate=0,cp=0,minbucket=1,minsplit=2))
###Pruning the maximal tree to minimize xerror
#mymodel$cptable
xerr<-mymodel$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-mymodel$cptable[minxerr,"CP"]
mymodel.prune<-prune(model,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
###Predict on the test set
mymodel.prune.predict <- predict(mymodel.prune, newdata=data.test, type="class")
besttable<-table(data.test$Churn, mymodel.prune.predict,dnn=c("Actual", "Predicted"))
besterror=(besttable[2]+besttable[3])/nrow(data.test)
besttype1=besttable[3]/(besttable[1]+besttable[3])
besttype2=besttable[2]/(besttable[2]+besttable[4])

####Other Attempts####
#build the tree
tree.data=tree(Churn~.,data = data[train,])
mymodel <- rpart(Churn ~ .,data=data[train,],method="class",
                 parms=list(split="information"),control=rpart.control(minsplit=10, 
                                                                       minbucket=5,maxdepth=20,usesurrogate=0,maxsurrogate=0))

printcp(mymodel)
plot(mymodel)
text(mymodel,pretty=0)
mymodel.predict <- predict(mymodel, newdata=data.test, type="class")
mytable<-table(mydata.test$Churn, mymodel.predict,dnn=c("Actual", "Predicted"))
errorRate[1]=(mytable[2]+mytable[3])/nrow(mydata.test)
type1[1]=mytable[3]/(mytable[1]+mytable[3])
type2[1]=mytable[2]/(mytable[2]+mytable[4])

###Building a maximal model
mymodel<-rpart(Churn~.,data=data[train,],method="class",
               parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                     maxsurrogate=0,cp=0,minbucket=1,minsplit=2))
#fancyRpartPlot(mymodel, main="Maximal Decision Tree")
plot(mymodel)
#print(mymodel$cptable)
plotcp(mymodel)
grid()

#this next model is a copy of the best model
###Pruning the maximal tree to minimize xerror
#mymodel$cptable
xerr<-mymodel$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-mymodel$cptable[minxerr,"CP"]
mymodel.prune<-prune(mymodel,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
asRules(mymodel.prune)


###Predict on the test set
mymodel.prune.predict <- predict(mymodel.prune, newdata=mydata.test, type="class")
mytable2<-table(data.test$Churn, mymodel.prune.predict,dnn=c("Actual", "Predicted"))
errorRate[2]=(mytable2[2]+mytable2[3])/nrow(mydata.test)
type1[2]=mytable2[3]/(mytable2[1]+mytable2[3])
type2[2]=mytable2[2]/(mytable2[2]+mytable2[4])
#round(100*table(mydata.test$Churn, mymodel.prune.predict,dnn=c("% Actual", "% Predicted"))/length(mymodel.prune.predict))


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
#this one is clearly worse and has a higher error rate.

####Loss Matrix####
mymodel.prune<-rpart(Churn~.,data=data[train,],method="class",
                     parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                           maxsurrogate=0,cp=0,minbucket=1,minsplit=2,loss=matrix(c(0,1,10,0), byrow=TRUE, nrow=2)))
###Still want to prune even when using Loss Matrix to build maximal tree
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
#so it looks like using the loss matrix is helpful
#test different values for the loss matrix
#this may take about a minute to run
loss=seq(0,10)
#extraErrorRate=list()
output=matrix(NA,11,11)
for (j in loss){
  for(i in loss){

    mymodel.prune<-rpart(Churn~.,data=data[train,],method="class",
                         parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                               maxsurrogate=0,cp=0,minbucket=1,minsplit=2,loss=matrix(c(0,i,j,0), byrow=TRUE, nrow=2)))
    xerr<-mymodel.prune$cptable[,"xerror"]
    minxerr<-which.min(xerr)
    mincp<-mymodel.prune$cptable[minxerr,"CP"]
    mymodel.prune<-prune(mymodel.prune,cp=mincp)
    mymodel.prune.predict <- predict(mymodel.prune, newdata=data.test, type="class")
    mytable=table(data.test$Churn, mymodel.prune.predict,dnn=c("Actual", "Predicted"))
    output[j,i]=(mytable[2]+mytable[3])/nrow(data.test)

  }

}


