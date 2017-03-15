cat("\f")
remove(list = ls())
set.seed(1)
setwd("C:/MSBA/Spring2017/MachineLearning2/Lab3")
library(randomForest)
library(tree)
library(gbm)
library(dplyr)

data <- read.csv("lab3data.csv")
data <- na.omit(data)
data <- data[-1]
names(data) <- tolower(names(data)) 
names(data)
head(data$churn)
train <-  sample(1:nrow(data), .8 * nrow(data))

data.train <- data[train,]
data.test <- data[-train,]
train.churn <- as.vector(data[train,]$churn)
test.churn <-  data[-train,"churn"]

str(data)
#head(data)
###### RANDOM FOREST #####
nvars = seq(2,(ncol(data)-1))
train.errorsI = rep(NA, length(nvars))
train.errorsII = rep(NA, length(nvars))
test.errorsI = rep(NA, length(nvars))
test.errorsII = rep(NA, length(nvars))
train.error <- rep(NA, length(nvars))
test.error <- rep(NA, length(nvars))

for(i in 1:length(nvars)){
      rf.fit <- randomForest(churn~., data=data, subset=train, mtry=(i+1), importance =TRUE)
      pred.rf.train <- predict(rf.fit, newdata=data[train ,])
      pred.rf.test <- predict(rf.fit, newdata=data[-train ,])
      
      train.rf.tables <- table(pred.rf.train, train.churn)
      test.rf.tables <- table(pred.rf.test, test.churn)
      
      train.error[i] <-  (train.rf.tables[2] + train.rf.tables[3])/nrow(data[train,])
      train.errorsII[i] <- train.rf.tables[3]/(train.rf.tables[3]+train.rf.tables[4])
      train.errorsI[i] <- train.rf.tables[2]/(train.rf.tables[1]+train.rf.tables[2])
      
      test.error[i] <-  (test.rf.tables[2] + test.rf.tables[3])/nrow(data[-train,])
      test.errorsII[i] <- test.rf.tables[3]/(test.rf.tables[3]+test.rf.tables[4])
      test.errorsI[i] <- test.rf.tables[2]/(test.rf.tables[1]+test.rf.tables[2])
      
}
plot(nvars, train.error, type = "b", xlab = "nvars", ylab = "Train MSE", col = "blue", pch = 20)
plot(nvars, test.error, type = "b", xlab = "nvars", ylab = "Test MSE", col = "blue", pch = 20, las =2)

plot(nvars, train.errorsI, type = "b", xlab = "nvars", ylab = "Train I", col = "blue", pch = 20)
plot(nvars, train.errorsII, type = "b", xlab = "nvars", ylab = "Train II", col = "blue", pch = 20, las =2)



plot(nvars, test.errorsI, type = "b", xlab = "nvars", ylab = "Test I", col = "blue", pch = 20)
plot(nvars, test.errorsII, type = "b", xlab = "nvars", ylab = "Test II", col = "blue", pch = 20, las =2)

nvars[which.min(test.errorsII)]
min(test.errorsII)







