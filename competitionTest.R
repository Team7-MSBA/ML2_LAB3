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
#pred<-predict(object=boost.fit,newdata[-trainindices,1:19])
newtest<-read.csv("ChurnDataTest.csv") 
newfeature<-newtest[,-1]
customerID<-newtest[,1]
churn<-predict(object=boost.fit,newfeature)
#mytable<-table(newdata[-trainindices,20],pred)
# mean(pred!=newdata[-trainindices,20])
# overall error rate is 18.72%
result<-data.frame(customerID,churn)

write.csv(result,file="Team7Prediction.csv",row.names =F)