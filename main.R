#Author & date: Navin K Ipe. July 2018
#License: MIT
#Improving ARIMA model

#tsclean() for outlier removal and inputting missing values
#decompose() or stl() to examine and possibly remove components of the series
#adf.test(), ACF, PACF for stationarity check and order of differencing

library(datasets)
library(tseries)
library(zoo)
library(expsmooth)
library(ggplot2)
require(graphics)
#library(neuralnet)
#library(RSNNS)
library(nnfor)
library(rstudioapi);rstudioapi::getActiveDocumentContext()$path#set path as current working directory
#if(!is.null(dev.list())) dev.off()#clear old plots
cat("\f")#clear console
#rm(list = setdiff(ls(), lsf.str()))#remove all objects except functions
#rm(list=ls())#remove all objects loaded into R
#rm(list=lsf.str())#remove all functions but not variables
#rm(list=ls(all=TRUE))#clear all variables

# fileName = "monthly-water-usage-mlday-london.csv"
# dd<-read.csv(fileName, header=TRUE, sep=",", stringsAsFactors=FALSE)
# d <- ts(dd[,c("Usage")])
# d <- tsclean(d)
# dd$ma5 <- ma(d, order = 5)
# plot(d)
# lines(dd$ma5)
# d=d[2:nrow(d),];
# d=transform(d,V2=as.double(V2))
d = datasets::AirPassengers#for strong seasonality and trend
#d = datasets::austres#trend but no seasonality
#d = datasets::BJsales#exponential growth trend
#d = datasets::JohnsonJohnson#trend and seasonality
#d = datasets::nhtemp#almost stationary already
#d = datasets::USAccDeaths#strong seasonality but no trend
#plot(d, main="Data")

stYr = 1; enYr = 150
trainStYr = stYr; trainEnYr = enYr - 50;
testStYr = trainEnYr; testEnYr = enYr;
longPredAhead = 15
shortPredAhead = 5
meanRng = 3;#25;#window size for mean
timePer = 1#time period

plot(d, main="BJSales",ylab="sales")
if (isTRUE(isStationary(d))) {print("is stationary")} else {print("is not stationary")}
showACFPlots(d)
showACFPlots(diff(d))

#---dividing into test and training sets (test data is not needed in this program)
colorOrder=c("black", "red")
tr = window(d, start=trainStYr, end=c(trainEnYr, timePer))
te = window(d, start=testStYr+1, end=c(testEnYr, timePer))
ts.plot(tr, te, lty = c(4,3), col=colorOrder, ylab="people", main="Train.Test")
legend("topleft", legend=c("training", "test"), col=colorOrder, lty=1:2, cex=0.8)

#---predict with normal arima
arimaParam = c(2, 1, 4)
#print(cat("Using arima: ", autoF5$method))
fit <- arima(tr, arimaParam, seasonal = list(order = arimaParam, period = timePer))
print(cat("MyArima: AIC:", fit$aic, ", AICC:", fit$aicc, ", ARMA:", fit$arma, ", BIC:", fit$bic))
fitResid <- fit$residuals
plot(fitResid, main="MyArima residuals")
oriPred5 <- predict(fit, n.ahead = shortPredAhead)
oriPred15 <- predict(fit, n.ahead = longPredAhead)
m5 <- meanSquaredError(te[1:shortPredAhead], oriPred5$pred[1:shortPredAhead])
m15 <- meanSquaredError(te[1:shortPredAhead], oriPred15$pred[1:longPredAhead])
plot(m15, xlab="time", ylab="error", main="MSE MyArima", lty=2, col="red");lines(m5, lty=3, col="blue");legend("topleft", legend=c("pred15", "pred5"), col=c("red", "blue"), lty=1:2, cex=0.8)

#---auto arima
aa <- auto.arima(tr)
print(cat("AutoArima: AIC:", aa$aic, ", AICC:", aa$aicc, ", ARMA:", aa$arma, ", BIC:", aa$bic))
aaFitted <- aa$fitted; aaResid <- aa$residuals
plot(aaResid, main="AutoArima residuals")
#par(mfrow=c(2,1))#set for subplot
plot(fitResid, main="Residuals", lty=3,col="red");lines(aaResid, lty=4,col="blue");legend("topleft", legend=c("MyArima", "AutoArima"), col=c("red", "blue"), lty=1:2, cex=0.8)
#par(mfrow=c(1,1))#reset to single plots
plot(d, lty=c(3), col="black");lines(aaFitted, lty=c(3), main="AutoArimaFitted", ylab="people", col="blue");legend("topleft", legend=c("fitted", "original"), col=c("blue", "black"), lty=1:2, cex=0.8)
autoF5 <- forecast(aa,h=5)
autoF15 <- forecast(aa,h=15)
print(cat("Auto arima estimated for F5: ", autoF5$method));print(cat("Auto arima estimated for F15: ", autoF15$method))

#---plot all pred
colorOrder = c("black","green","red","blue","cyan")
ts.plot(d, oriPred5$pred, oriPred15$pred, autoF5$mean, autoF15$mean, log = "y", lty = c(3,2,3,6,7), col=colorOrder, ylab="people", main="Predictions");legend("topleft", legend=c("d","ori5", "ori15", "auto5", "auto15"), col=colorOrder, lty=1:2, cex=0.8)
am5 <- meanSquaredError(te[1:shortPredAhead], autoF5$mean[1:shortPredAhead])
am15 <- meanSquaredError(te[1:shortPredAhead], autoF15$mean[1:longPredAhead])

#---trend
trnd <- rollmean(d, meanRng, align = c("left"))
plot(trnd, main="Trend")
#---residual
residu <- d[1:length(trnd)] - trnd;#residue = original - trend
plot(residu, main="Residue")

#---ANN
annTrain <- window(residu, end = trainEnYr+1)
annTest <- window(residu, start = testStYr)
autoplot(annTrain) + ylab("sales") + ggtitle("Training")
annFit <- mlp(annTrain, hd.auto.type = "valid")
annShortForecast <- forecast(annFit, h = shortPredAhead+1)
annLongForecast <- forecast(annFit, h = longPredAhead+1)
autoplot(annTest) + autolayer(annShortForecast, series = "short", linetype = "dotted") + autolayer(annLongForecast, series = "long", linetype = "dashed") + ylab("people")
plot(annFit)

#---predict trend with arima
trndTr <- window(trnd, start=trainStYr, end=c(trainEnYr, timePer))
if (isTRUE(isStationary(trndTr))) {print("is stationary")} else {print("is not stationary")}
showACFPlots(diff(diff(trndTr)))
arimaParam = c(11, 2, 10)
fit <- arima(trndTr, arimaParam, seasonal = list(order = arimaParam, period = timePer))
print(cat("TrendArima: AIC:", fit$aic, ", AICC:", fit$aicc, ", ARMA:", fit$arma, ", BIC:", fit$bic))
trndPred5 <- predict(fit, n.ahead = shortPredAhead)
trndPred15 <- predict(fit, n.ahead = longPredAhead)

#---sum up trend and residue predictions
trndTest <- trndPred15$pred[1:longPredAhead]
residuTest <- annLongForecast[1:longPredAhead]
#---get values
actual <- te[1:longPredAhead]
oriArima <- oriPred15$pred[1:longPredAhead]
sumTrRe <- trndTest + residuTest

plot(sqrt((actual-oriArima)^2), main="MSE. MyArima vs improved Arima", col="blue", lty=3, xlab="forecast instance",ylab="MSE")
lines(sqrt((actual-sumTrRe)^2), col="red", lty=3)
print(cat("Sum MSE oriArima:",sum(sqrt((actual-oriArima)^2)),", sum MSE TrRe:",sum(sqrt((actual-sumTrRe)^2))))
legend("topleft", legend=c("actual-MyArima", "actual-sum(ANN+trend)"), col=c("blue","red"), lty=1:2, cex=0.8)

meanSquaredError <- function(original, predicted) {
    original = na.trim(original)
    predicted = na.trim(predicted)
    m = sqrt((original - predicted)^2)
    return (m)
}

# A function to do exponential smoothing:
exponential.smooth <- function(x, lambda)
{
    if(length(lambda) > 1) {stop("lambda must be a single number")}
    if(lambda > 1 || lambda <= 0) {stop("lambda must be between zero and one")}
    xlam <- x * lambda
    xlam[1] <- x[1]
    filter(xlam, filter = 1 - lambda, method = "rec")
}

isStationary <- function(d) {
    ad <- adf.test(d)
    print(cat("adf.test p value: ", ad$p.value))
    if (ad$p.value < 0.05) {res <- TRUE} else {res <- FALSE}
    return (res)
}

showACFPlots <- function(d) {
    pacf(d, main="PACF")
    acf(d, main="ACF")
}

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
