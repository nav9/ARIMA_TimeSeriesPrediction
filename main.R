#Author & date: Navin K Ipe. July 2018
#License: MIT
#Improving ARIMA model

library(datasets)
library(tseries)
library(zoo)
library(expsmooth)
require(graphics)
if(!is.null(dev.list())) dev.off()#clear old plots
cat("\f")#clear console
#rm(list = setdiff(ls(), lsf.str()))#remove all objects except functions
#rm(list=ls())#remove all objects loaded into R
#rm(list=lsf.str())#remove all functions but not variables
#rm(list=ls(all=TRUE))#clear all variables

# fileName = "annual-changes-in-global-tempera.csv"
# #fileName = "monthly-water-usage-mlday-london.csv"
# d<-as.data.frame(read.csv(fileName, header=FALSE, sep=","))
# d=d[2:nrow(d),];
# d=transform(d,V2=as.double(V2))
d = datasets::AirPassengers#for strong seasonality and trend
#d = datasets::austres#trend but no seasonality
#d = datasets::BJsales#exponential growth trend
#d = datasets::JohnsonJohnson#trend and seasonality
#d = datasets::nhtemp#almost stationary already
#d = datasets::USAccDeaths#strong seasonality but no trend

testLim = 40
predAhead = 20
plot(d, main="Main plot", xlab="years", ylab="temperature")
abline(reg=lm(d~time(d)))
message("Type:",class(d))
message("Starts at:",start(d))
message("Ends at:",end(d))
message("Frequency:",frequency(d))
print(summary(d))#cycle(d)
plot(aggregate(d,FUN=mean))#shows yearly trend
boxplot(d~cycle(d))#plot across months. Shows seasonality
plot(log(d), main="log(d) removes unequal variances", xlab="years", ylab="temperature")
plot(diff(log(d)), main="diff(log(d) de-trends", xlab="years", ylab="temperature")
#plot(log(diff(d)), main="As log(diff(d))", xlab="years", ylab="temperature")
adft = adf.test(diff(log(d)), alternative="stationary", k=0)
acf(diff(log(d)))
pacf(diff(log(d)))
fit <- arima(log(d), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 15*12)
ts.plot(d,exp(pred$pred), log = "y", lty = c(1,3), col=c("blue","red"))#Plots multiple time series on same plot. lty is line type 1 for first time series. 3 for second time series

#---dividing into test and training sets
tr = window(d, start=1949, end=c(1955,12))
te = window(d, start=1956, end=c(1960,12))
#---checking for stationarity and predict
acf(diff(log(d)))
pacf(diff(log(d)))
fitTr <- arima(log(tr), c(1,1,2),seasonal=list(order=c(1,1,2),period=12))
trPred <- predict(fitTr, n.ahead=5*12)
ts.plot(d,tr,exp(trPred$pred), log = "y", lty = c(3,3,3), col=c("green","blue","red"))#Plots multiple time series on same plot. lty is line type 1 for first time series. 3 for second time series
#---measure errors
p1 = exp(trPred$pred[1:(5*12)])
p2 = te[1:(5*12)]
er = p1 - p2



#converting to time series
#series<-ts(d,frequency=12,start=c(1969,1))
#If data was uploaded as a dataframe, specify the col with precision
#series<-ts(dat[[1]], frequency=12,start=c(1969,1))


# d=rollmean(d,15)
# plot(d, main="Mean plot", xlab="years", ylab="temperature", col="red")
# d=diff(d)
# plot(d, main="DiffMean plot", xlab="years", ylab="temperature", col="green")
#
# # d<- HoltWinters(d,beta=FALSE,gamma=FALSE, seasonal="multiplicative")
# # lines(fitted(d)[,1], col=3)
#
#
#
# acf(d); pacf(d);
#
# ad = adf.test(d)
# dat = d[1:testLim]
# ar = arima(dat, c(1,0,8))
# ar2 = arima(dat, c(2,0,9))
# pr = predict(ar, n.ahead=predAhead)
# pr2 = predict(ar2, n.ahead=predAhead)
# plot(dat, type="o")
# asd = c(d[testLim+1], pr$pred)
# lines(seq(testLim,length(d)), c(d[testLim+1], pr$pred), type="o", col="green")
# lines(seq(testLim,length(d)), c(d[testLim+1], pr$pred), type="o", col="red")
#
# dat = na.trim(d[testLim+1:length(d)])
# meanSquaredError(dat, pr$pred)
#
#
# meanSquaredError <- function(original, predicted) {
#     original = na.trim(original)
#     predicted = na.trim(predicted)
#     original
#     predicted
#     m = (original - predicted)^2
#     plot(m, main="MSE", xlab="error")
#     print(m)
# }

# a. Original dataset
# b. Apply any filter to get trend dataset
# c. Residual data set = original dataset - trend dataset
# d. Now got two datasets 1. Residual dataset and 2. trend datasets
# e. Apply ARIMA for the above two datasets separately or apply ARIMA
#     for one dataset and other technique/s (ANN, fuzzy etc) on
#     the other dataset. Use your discretion which one applies as per your
#     previous knowledge.
# f. Add the obtained predictions to get final predications
# g. Now consider the original dataset and apply arima on it.
# h. compare f and g.
# i. For more details see the papers on it. There are many Eg: Predictive
#     data mining on average global temperature using variants of ARIMA models.
#MAPE (Mean Absolute Percentage Error), MaxAPE (Maximum Absolute Percentage Error) and MAE (Mean Absolute Error) have been used as a performance measures to compare between the models

# #single exponential smoothing
# library(expsmooth)
# data(unemp.cci)
# cci <- ts(unemp.cci[,"cci"],start=c(1997))
# plot.ts(cci)
# cci.smooth<- HoltWinters(cci, beta=FALSE, gamma=FALSE)
# plot(cci.smooth$fitted)
#
# Double Exponential Smoothing
# The following R code performs double-exponential smoothing:
#     cci.smoother<- HoltWinters(cci, gamma=FALSE)
# plot(cci.smoother$fitted)

# https://stats.stackexchange.com/questions/214296/lag-free-filter-methods-for-time-series
# library(signal)
# matplot(data.frame( beaver1[,3],
#                     runmed(beaver1[,3], k = 11),
#                     filter(filt = sgolay(p = 5, n = 11), x = beaver1[,3])
# ), type='l', lwd=2, lty=1, ylab='')
# legend('topleft', legend=c('original', 'runmed', 'Savitzky–Golay'), col=1:3, lty=1:3, lwd=2)


# ## an ARIMA fit
# fit3 <- arima(presidents, c(3, 0, 0))
# predict(fit3, 12)
# ## reconstruct this
# pr <- KalmanForecast(12, fit3$model)
# pr$pred + fit3$coef[4]
# sqrt(pr$var * fit3$sigma2)
# ## and now do it year by year
# mod <- fit3$model
# for(y in 1:3) {
#     pr <- KalmanForecast(4, mod, TRUE)
#     print(list(pred = pr$pred + fit3$coef["intercept"],
#                se = sqrt(pr$var * fit3$sigma2)))
#     mod <- attr(pr, "mod")
# }
