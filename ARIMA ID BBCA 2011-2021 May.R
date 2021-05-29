library(fpp)

rawdata <- read.csv("C:/Users/Fernando/Documents/Dataset/Stocks/Indonesia/Bluechip/BBCA/BBCA.JK 2011-2021 May.csv", header = TRUE)
rawdata

plot(rawdata[,6], type ="l")

data <- ts(rawdata[,6], start = c(2011,6), frequency = 12)
data
tsdisplay(diff(log(data)))

#DATA SPLIT
data_train = window(data, start = c(2011,6), end = c(2018,12), frequency = 12)
data_train
plot(data_train)

data_test = window(data, start = c(2019), frequency = 12)
data_test

#STATIONARITY CHECK
ggtsdisplay(data_train)
adf.test(data, alternative ="stationary")

#FIRST DIFFERENCING AND MDOEL IDENTIFICATION
ggtsdisplay(diff(data_train))
adf.test(diff(data_train), alternative = "stationary")

#MODEL
model1 <- Arima(data_train, order = c(0,1,0), include.drift = TRUE)
model2 <- Arima(data_train, order = c(0,1,0), include.drift = FALSE)
model3 <- Arima(data_train, order = c(1,1,0), include.drift = TRUE)
model4 <- Arima(data_train, order = c(0,1,1), include.drift = TRUE)
model5 <- Arima(data_train, order = c(1,1,1), include.drift = TRUE)


#RESIDUALS CHECK
res1 <- residuals(model1)
res2 <- residuals(model2)
res3 <- residuals(model3)
res4 <- residuals(model4)
res5 <- residuals(model5)


ggtsdisplay(res1)
ggtsdisplay(res2)
ggtsdisplay(res3)
ggtsdisplay(res4)
ggtsdisplay(res5)


Box.test(res1, lag = 12, type = "Ljung-Box", fitdf = 1)
Box.test(res2, lag = 12, type = "Ljung-Box", fitdf = 1)
Box.test(res3, lag = 12, type = "Ljung-Box", fitdf = 1)
Box.test(res4, lag = 12, type = "Ljung-Box", fitdf = 1)
Box.test(res5, lag = 12, type = "Ljung-Box", fitdf = 1)


#FORECAST
fc1 <- forecast(model1, h = 32)
fc2 <- forecast(model2, h = 32)
fc3 <- forecast(model3, h = 32)
fc4 <- forecast(model4, h = 32)
fc5 <- forecast(model5, h = 32)


fc1
fc2
fc3
fc4
fc5


#PLOT
plot(data, ylim = c(0,45000), main ="BBCA Stock Price 2011-2021", ylab = "BBCA", xlab = "Year")
lines(fc1$mean, col = 2)
lines(fc2$mean, col = 3)
lines(fc3$mean, col = 4)
lines(fc4$mean, col = 5)
lines(fc5$mean, col = 6)

plot(data, ylim = c(0,45000))
lines(fc1$mean, col = 2)
lines(fc1$lower[,2], col = 3, lty = 2)
lines(fc1$upper[,2], col = 3, lty = 2)

#MODEL ACCURACY
e1 <- mean(abs(data_test-fc1$mean))
e2 <- mean(abs(data_test-fc2$mean))
e3 <- mean(abs(data_test-fc3$mean))
e4 <- mean(abs(data_test-fc4$mean))
e5 <- mean(abs(data_test-fc5$mean))

e1
e2
e3
e4
e5

MAE <- cbind(e1,e2,e3,e4,e5)
MAE

e12 <- mean((data_test-fc1$mean)^2)
e22 <- mean((data_test-fc2$mean)^2)
e32 <- mean((data_test-fc3$mean)^2)
e42 <- mean((data_test-fc4$mean)^2)
e52 <- mean((data_test-fc5$mean)^2)

e12
e22
e32
e42
e52

MSE <- cbind(e12, e22, e32, e42, e52)
MSE

#INFORMATION CRITERION
ic1 <- cbind(model1$bic, model2$bic, model3$bic, model4$bic, model5$bic)
ic1

#FULL DATA MODELLING
model <- Arima(data, order = c(1,1,0), include.drift = TRUE)
model

res <- residuals(model)
tsdisplay(res)
Box.test(res, lag = 12, type = "Ljung-Box", fitdf = 2)

#STATIONARITY CHECK
tsdisplay(data)
adf.test(data, alternative ="stationary")

#FULL DATA FORECAST
fc <- forecast(model, h = 36)
fc

plot(data, main ="BBCA.JK 5 year Forecast", ylim = c(0, 50000), xlim = c(2011,2025))
lines(fc$mean, col = 2)




