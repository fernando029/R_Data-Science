data <- read.csv("C:/Users/Fernando/Documents/Dataset/IceCreamData.csv")
set.seed(2)
library(caTools)
library(DMwR2)
plot(data$Revenue, data$Temperature)

#Training and Test Sets Method 1
split <- sample.split(data, SplitRatio = 0.8)
split
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)
train
test
 

#Training and Test Sets Method 2
train <- data[1:350,]
test <- data[351:500,]
test
train

#Training and Test Sets Method 3 (SAMPLING)
trainRowIndex <- sample(1:nrow(data), 0.8*nrow(data))
train <- data[trainRowIndex,]
test <- data[-trainRowIndex,]
train
test

#Model
Model <- lm(Revenue ~ Temperature,data=train)
summary(Model)

#Prediction
pred <- predict(Model,test)
pred

#Comparison
plot(pred, type ='l', col = 1)
lines(test$Revenue, type ="l", col = 3)

#Accuracy
RMSE = mean((pred-test$Revenue)^2)
RMSE
actuals_predicts = data.frame(cbind(test$Revenue, pred))
actuals_predicts
head(actuals_predicts)
cor(actuals_predicts)
