#Author & date: Navin K Ipe. July 2018
#License: MIT
#Improving ARIMA model

library(datasets)
library(tseries)

#d = datasets::AirPassengers#for strong seasonality and trend
#d = datasets::austres#trend but no seasonality
#d = datasets::BJsales#exponential growth trend
#d = datasets::JohnsonJohnson#trend and seasonality
d = datasets::nhtemp#almost already stationary
#d = datasets::USAccDeaths#strong seasonality

testLim = 40
predAhead = 20
#plot(d, main="Main plot", xlab="years", ylab="temperature")
acf(d)
pacf(d)
ad = adf.test(d)
dat = d[1:testLim]
ar=arima(dat, c(1,0,8))
ar2=arima(dat, c(2,0,9))
pr = predict(ar, n.ahead=predAhead)
pr2 = predict(ar2, n.ahead=predAhead)
plot(dat, type="o")
asd=c(d[testLim+1], pr$pred)
lines(seq(testLim,length(d)), c(d[testLim+1], pr$pred), type="o", col="green")
lines(seq(testLim,length(d)), c(d[testLim+1], pr$pred), type="o", col="red")


meanSquaredError <- function(original, predicted) {

}
