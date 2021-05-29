library(fpp)

data <- read.csv("C:/Users/Fernando/Documents/Dataset/Economics/World Bank/Indonesia/indonesia-inflation-rate-cpi 1960-2019.csv", header = TRUE)
data

#CONVERT TO TS
data <- ts(data[,-2], start =1960, frequency = 1)
data
plot(data)

tsdisplay(data)
adf.test(data, alternative = "stationary")

#FIRST DIFFERENCING
tsdisplay(diff(data))
adf.test(diff(data), alternative = "stationary")

##          STATIONARY AFTER FIRST DIFFERENCE             ##


#DATA SPLIT
data_train = window(data, start = 1960, end = 2013)
data_train

data_test = window(data, start = 2014)
data_test

#STATIONARITY CHECK
tsdisplay(data_train)
adf.test(data_train, alternative = "stationary")

#FIRST DIFFERENCING
tsdisplay(diff(data_train))
adf.test(diff(data_train), alternative = "stationary")

#MODEL IDENTIFICATION
tsdisplay(diff(data_train))

model1 <- Arima(data_train, order = c(0,1,0))
model2 <- Arima(data_train, order = c(1,1,0))
model3 <- Arima(data_train, order = c(0,1,1))
model4 <- Arima(data_train, order = c(1,1,1))

model1
model2
model3
model4

res1 <- residuals(model1)
res2 <- residuals(model2)
res3 <- residuals(model3)
res4 <- residuals(model4)

tsdisplay(res1)
tsdisplay(res2)
tsdisplay(res3)
tsdisplay(res4)

#ERRORS AUTOCORRELATION CHECK
Box.test(res1, lag=16, fitdf=2, type="Ljung")
Box.test(res2, lag=16, fitdf=2, type="Ljung")
Box.test(res3, lag=16, fitdf=2, type="Ljung")
Box.test(res4, lag=16, fitdf=2, type="Ljung")

#FORECAST
fc1 <- forecast(model1, h=6)
fc2 <- forecast(model2, h=6)
fc3 <- forecast(model3, h=6)
fc4 <- forecast(model4, h=6)

#FORECAST PLOT
plot(data, ylim =c(-50, 200), xlim = c(1961,2019))
lines(fc1$mean, col=2)
lines(fc2$mean, col=3)
lines(fc3$mean, col=4)
lines(fc4$mean, col=5)

#MODEL ACCURACY
e1 <- mean(abs(data_test-fc1$mean))
e2 <- mean(abs(data_test-fc2$mean))
e3 <- mean(abs(data_test-fc3$mean))
e4 <- mean(abs(data_test-fc4$mean))


e1
e2
e3
e4


MAE <- cbind(e1,e2,e3,e4)
MAE

e12 <- mean((data_test-fc1$mean)^2)
e22 <- mean((data_test-fc2$mean)^2)
e32 <- mean((data_test-fc3$mean)^2)
e42 <- mean((data_test-fc4$mean)^2)


e12
e22
e32
e42


MSE <- cbind(e12, e22, e32, e42)
MSE

#INFORMATION CRITERION
icic <- cbind(model1$bic, model2$bic, model3$bic, model4$bic)
icic

#FULL DATA FORECAST
model <- Arima(data, order = c(1,1,0))

res <- residuals(model)

tsdisplay(res)


#ERRORS AUTOCORRELATION CHECK
Box.test(res, lag=16, fitdf=2, type="Ljung")


#FORECAST
fc <- forecast(model, h=6)

#FORECAST PLOT
plot(data, ylim =c(-50, 200), xlim = c(1961,2025))
lines(fc$mean, col=2)

