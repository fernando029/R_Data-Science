

library(forecast); 
library(stats); # the packages used

############ Tutorial activity 2 #############

crude.oil <- read.delim("crude.txt", header=T)
attach(crude.oil)
crude.pr <- ts(crude)
crude.rets <- 100*diff(log(crude.pr))
plot(crude.pr, main="Crude oil for the year 2016")
plot(crude.rets, main="Crude oil returns")
is.ts(crude.pr)
is.ts(crude.rets)


fit.arima <- Arima(crude.rets[1:201], order=c(1,0,1), include.mean=T)
print(fit.arima)
forc.crude = predict(fit.arima, n.ahead=5)
print(forc.crude)
residuals(fit.arima)
fitted(fit.arima)

coef(fit.arima)

## R calculates the forecasts as follows

fc1 = 0.1703*(1-(-0.6093)) + (-0.6093)*(2.1124) +  0.6999*(1.9346)

fc2 = 0.1703*(1-(-0.6093)) + (-0.6093)*fc1

fc3 = 0.1703*(1-(-0.6093)) + (-0.6093)*fc2

fc4 = 0.1703*(1-(-0.6093)) + (-0.6093)*fc3

fc5 = 0.1703*(1-(-0.6093)) + (-0.6108313)*fc4

forecasts <- c(fc1, fc2, fc3, fc4, fc5); 
print(forecasts)

fc <- forecast(fit.arima, h=5)
print(fc)

accuracy(f=c(fc[[4]][[1]], fc[[4]][[2]], fc[[4]][[3]], fc[[4]][[4]], fc[[4]][[5]]), x=tail(crude.rets, 5))

accuracy(f=c(0.34113448, 0.06624718, 0.23372275, 0.13168797, 0.19385284), 
         x=c(-1.41651001, -1.33641163, -0.04016871, -0.24135169, 0.00000000))

############ Tutorial activity 3 #############


#### i. white noise #### 

set.seed(600)
whitenoise = rnorm(n=500, m=0, sd=1)
acf(whitenoise); pacf(whitenoise)


#### ii AR(2) model ###

library(forecast)

set.seed(645)
ar2.sim <- arima.sim(list(order=c(2,0,0), ar=c(0.8, 0.15)), n=500)

ar2.sim.plot <- plot(arima.sim(list(order=c(2,0,0), ar=c(0.8, 0.05)), n=500), ylab="x", 
     main=(expression(AR(2)~ ~ ~ phi_1==+.8 ~ ~ ~ phi_2==+.05)))

Acf(ar2.sim, 15)
Pacf(ar2.sim, 15)

#### iii MA(1) model ###

library(forecast)

set.seed(2251)
ma1.sim <- arima.sim(list(order=c(0,0,1), ma=c(0.85)), n=500)
Acf(ma1.sim, 15)
Pacf(ma1.sim, 15)


#### vi ARMA(2,1) model ###

library(forecast)

set.seed(540)
arma21.sim <- arima.sim(list(order=c(2,0,1), ar=c(0.75, 0.16), ma=c(0.8)), n=500)
Acf(arma21.sim, 15)
Pacf(arma21.sim, 15)


# Process -	White noise	      	                         
## No significant coefficients - acf	
## No significant coefficients - pacf

# Process -	AR(2)	
## Geometrically declining or damped sinusoid - acf	
## First 2 pacf coefficients significant, all others insignificant - pacf

# Process -	MA(1)	
## First acf coefficient significant, all others insignificant - acf	
## Geometrically declining or damped sinusoid - pacf

# Process -	ARMA(2,1)
## ARMA(2,1)	Geometrically declining or damped sinusoid - acf	
## Geometrically declining or damped sinusoid - pacf

