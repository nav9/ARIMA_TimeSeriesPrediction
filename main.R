#Author & date: Navin K Ipe. July 2018
#License: MIT
#Improving ARIMA model

library(datasets)
library(tseries)
library(zoo)
if(!is.null(dev.list())) dev.off()#clear old plots
cat("\f")#clear console
#rm(list = setdiff(ls(), lsf.str()))#remove all objects except functions
#rm(list=ls())#remove all objects loaded into R
#rm(list=lsf.str())#remove all functions but not variables
#rm(list=ls(all=TRUE))#clear all variables

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
ar = arima(dat, c(1,0,8))
ar2 = arima(dat, c(2,0,9))
pr = predict(ar, n.ahead=predAhead)
pr2 = predict(ar2, n.ahead=predAhead)
plot(dat, type="o")
asd = c(d[testLim+1], pr$pred)
lines(seq(testLim,length(d)), c(d[testLim+1], pr$pred), type="o", col="green")
lines(seq(testLim,length(d)), c(d[testLim+1], pr$pred), type="o", col="red")

dat = na.trim(d[testLim+1:length(d)])
meanSquaredError(dat, pr$pred)


meanSquaredError <- function(original, predicted) {
    original = na.trim(original)
    predicted = na.trim(predicted)
    original
    predicted
    m = (original - predicted)^2
    plot(m)
}
