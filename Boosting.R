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

################################ Boosting (use 80% of data set to train) ####################################

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
control <- trainControl(method='cv', number=4, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
boost.fit<-train(train[,1:19],train[,20],method="gbm",trControl=control,metric="ROC",preProc = c("center","scale"))
summary(boost.fit)
# The most important variables are contract and tenure. Internet service, online security, tech support, and payment method are important as well.
print(boost.fit)
# The last printout line shows that the tuning parameters the number of trees is 150, interation depth is 1,lambda is 0.1. 
plot(boost.fit)
pred<-predict(object=boost.fit,newdata[-trainindices,1:19])
mean(pred!=newdata[-trainindices,20])
# overall error rate is 19.72%


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


