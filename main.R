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
#if(!is.null(dev.list())) dev.off()#clear old plots
cat("\f")#clear console
#rm(list = setdiff(ls(), lsf.str()))#remove all objects except functions
#rm(list=ls())#remove all objects loaded into R
#rm(list=lsf.str())#remove all functions but not variables
#rm(list=ls(all=TRUE))#clear all variables
library(rstudioapi);rstudioapi::getActiveDocumentContext()$path#set path as current working directory

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

stYr = 1949; enYr = 1960
trainStYr = stYr; trainEnYr = enYr - 4;
testStYr = trainEnYr; testEnYr = enYr;
longPredAhead = 15
shortPredAhead = 5
meanRng = 3;#25;
timePer = 12#time period

if (isTRUE(isStationary(d))) {print("is stationary")} else {print("is not stationary")}
#showACFPlots(d)
showACFPlots(diff(d))

#---dividing into test and training sets
colorOrder=c("black", "red")
tr = window(d, start=trainStYr, end=c(trainEnYr, timePer))
te = window(d, start=testStYr+1, end=c(testEnYr, timePer))
ts.plot(tr, te, lty = c(4,3), col=colorOrder, ylab="people", main="Train.Test")
legend("topleft", legend=c("training", "test"), col=colorOrder, lty=1:2, cex=0.8)

#---predict with normal arima
arimaParam = c(1, 2, 2)
print(cat("Using arima: ", autoF5$method))
fit <- arima(tr, arimaParam, seasonal = list(order = arimaParam, period = timePer))
print(cat("MyArima: AIC:", fit$aic, ", AICC:", fit$aicc, ", ARMA:", fit$arma, ", BIC:", fit$bic))
fitResid <- fit$residuals
oriPred5 <- predict(fit, n.ahead = shortPredAhead)
oriPred15 <- predict(fit, n.ahead = longPredAhead)
m5 <- meanSquaredError(te[1:shortPredAhead], oriPred5$pred[1:shortPredAhead])
m15 <- meanSquaredError(te[1:shortPredAhead], oriPred15$pred[1:longPredAhead])
plot(m15, xlab="time", ylab="error", main="MSE MyArima", lty=2, col="red");lines(m5, lty=3, col="blue");legend("topleft", legend=c("pred15", "pred5"), col=c("red", "blue"), lty=1:2, cex=0.8)

# #---auto arima
# aa <- auto.arima(tr)
# print(cat("AutoArima: AIC:", aa$aic, ", AICC:", aa$aicc, ", ARMA:", aa$arma, ", BIC:", aa$bic))
# aaFitted <- aa$fitted; aaResid <- aa$residuals
# #par(mfrow=c(2,1))#set for subplot
# plot(fitResid, main="Residuals", lty=3,col="red");lines(aaResid, lty=4,col="blue");legend("topleft", legend=c("MyArima", "AutoArima"), col=c("red", "blue"), lty=1:2, cex=0.8)
# #par(mfrow=c(1,1))#reset to single plots
# plot(d, lty=c(3), col="black");lines(aaFitted, lty=c(3), main="AutoArimaFitted", ylab="people", col="blue");legend("topleft", legend=c("fitted", "original"), col=c("blue", "black"), lty=1:2, cex=0.8)
# autoF5 <- forecast(aa,h=5)
# autoF15 <- forecast(aa,h=15)
# print(cat("Auto arima estimated for F5: ", autoF5$method));print(cat("Auto arima estimated for F15: ", autoF15$method))
#
# #---plot all pred
# colorOrder = c("black","green","red","blue","cyan")
# ts.plot(d, oriPred5$pred, oriPred15$pred, autoF5$mean, autoF15$mean, log = "y", lty = c(3,2,3,6,7), col=colorOrder, ylab="people", main="Predictions");legend("topleft", legend=c("d","ori5", "ori15", "auto5", "auto15"), col=colorOrder, lty=1:2, cex=0.8)
# am5 <- meanSquaredError(te[1:shortPredAhead], autoF5$mean[1:shortPredAhead])
# am15 <- meanSquaredError(te[1:shortPredAhead], autoF15$mean[1:longPredAhead])

#---trend
trnd <- rollmean(d, meanRng, align = c("left"))
plot(trnd, main="Trend")
#---residual
residu <- d[1:length(trnd)] - trnd;#residue = original - trend
plot(residu, main="Residue")

#---ANN
air.train <- window(residu, end = trainEnYr)
autoplot(air.train) + ylab("Passengers") + ggtitle("Training")
air.test <- window(residu, start = testStYr)
autoplot(air.test) + ylab("Passengers") + ggtitle("Testing")
# Fitting MLP model
air.fit.mlp <- mlp(air.train, hd.auto.type = "valid")
air.fcst.mlp <- forecast(air.fit.mlp, h = 35)
# Visualize model predictions
autoplot(air.test) + autolayer(air.fcst.mlp, series = "MLP w/ opt. hidden nodes", linetype = "dashed") + theme_minimal() + ylab("Number of Passengers")




# #prepare data
# s = seq(1:length(tr))
# incr = 1
# ddd = data.frame()
# while ((incr+timePer) < length(tr)) {
#     ddd <- rbind(ddd, tr[incr:(incr+timePer)])#13 cols = 12 train + 1 target
#     incr = incr + 1;
# }
# #shuffle ddd
# ddd <- ddd[sample(1:nrow(ddd),length(1:nrow(ddd))), 1:ncol(ddd)]
#
# traValues <- ddd[,1:timePer]
# traTargets <- decodeClassLabels(ddd[,timePer+1])




# mlp.fit <- mlp(tr, tr)
# plot(mlp.fit$fitted.values)



## Creating index variable
# # Read the Data
# data = read.csv("cereals.csv", header=T)
#
# # Random sampling
# samplesize = 0.60 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
#
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
#
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
#
# # fit neural network
# set.seed(2)
# NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3 , linear.output = T )
# # plot neural network
# plot(NN)
# ## Prediction using neural network
#
# predict_testNN = compute(NN, testNN[,c(1:5)])
# predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)
# plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
# abline(0,1)
#
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5




# lines(c(trainEnYr+1,trainEnYr+1+(longPredAhead/12)), c(300,300))


# plot(d, main="Original plot", xlab="years", ylab="numPassenger", lty=c(3))
# lines(trnd, lty=c(1), col="red")
# resid <- d - trnd
# plot(resid, lty=c(1), col="green")

















#plot(decompose(d,"multiplicative"))
#compo <- stl(d, s.window="periodic")
#plot(compo, main="stl components", lty=c(4), col="red")#display season, trend, ramainder
#deSeas <- seasadj(decompose(d, "multiplicative"));
#plot(deSeas, main="seasonalityAdjusted")
#ad <- adf.test(deSeas, alternative="stationary");#The augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity. The null hypothesis assumes that the series is non-stationary. ADF procedure tests whether the change in Y can be explained by lagged value and a linear trend. If contribution of the lagged value to the change in Y is non-significant and there is a presence of a trend component, the series is non-stationary and null hypothesis will not be rejected.
#plot(decompose(deSeas, "multiplicative"))#why is seasonality still shown?



# lines(tsclean(d), main="Original plot", xlab="years", ylab="temperature", lty=c(4), col="red")
# plot(diff(d), main="Original plot", xlab="years", ylab="temperature", lty=c(4), col="red")
# abline(reg=lm(d~time(d)))
# message("Type:",class(d));message("Starts at:",start(d));message("Ends at:",end(d));message("Frequency:",frequency(d));print(summary(d))#cycle(d)
# plot(aggregate(d,FUN=mean))#shows yearly trend
# boxplot(d~cycle(d))#plot across months. Shows seasonality
# plot(log(d), main="log(d) removes unequal variances", xlab="years", ylab="temperature")
# plot(diff(log(d)), main="diff(log(d) de-trends", xlab="years", ylab="temperature")
# #plot(log(diff(d)), main="As log(diff(d))", xlab="years", ylab="temperature")
# adft = adf.test(diff(log(d)), alternative="stationary", k=0)

#---dividing into test and training sets
tr = window(d, start=trainStYr, end=c(trainEnYr, timePer))
te = window(d, start=trainEnYr, end=c(testEnYr, timePer))
# #---predict with normal arima
# acf(diff(log(tr)))
# pacf(diff(log(tr)))
# fit <- arima(log(tr), c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = timePer))
# oriPred5 <- predict(fit, n.ahead = shortPredAhead)
# oriPred15 <- predict(fit, n.ahead = longPredAhead)
# oriPred5$pred = exp(oriPred5$pred)
# oriPred15$pred = exp(oriPred15$pred)
# #---auto arima
# aa <- auto.arima(tr)
# autoF5 <- forecast(aa,h=5)
# autoF15 <- forecast(aa,h=15)
# #---plot all
# ts.plot(d, oriPred5$pred, oriPred15$pred, autoF5$Forecast, autoF15$Forecast, log = "y", lty = c(3,2,3,4,5), col=c("black","green","red","yellow","blue"))
# #---calculate moving average
# ma <- rollmean(d, 10, align = c("left"))
# plot(ma, lty=c(2), col=c("black"))
# lines(d, lty=c(3), col=c("red"))
# #---get arima for moving average
# acf(diff(diff(ma)))
# pacf(diff(diff(ma)))
# maFit <- arima(ma, c(2, 2, 1),seasonal = list(order = c(2, 2, 1), period = timePer))
# maPred5 <- predict(maFit, n.ahead = shortPredAhead)
# maPred15 <- predict(maFit, n.ahead = longPredAhead)
# #ts.plot(d, maPred5$pred, maPred15$pred, log = "y", lty = c(3,2,3), col=c("black","green","red"))
#---exponential smoothing
# es <- exponential.smooth(d, .1)
# plot(d, lty=c(3), col=c("red"))
# plot(diff(diff(es)), lty=c(2), col=c("black"))
# acf(diff(diff(es)))





# #---checking for stationarity and predict
# acf(diff(log(d)))
# pacf(diff(log(d)))
# fitTr <- arima(log(tr), c(1,1,2),seasonal=list(order=c(1,1,2),period=timePer))
# trPred <- predict(fitTr, n.ahead=shortPredAhead*timePer)
# ts.plot(d,tr,exp(trPred$pred), log = "y", lty = c(3,3,3), col=c("green","blue","red"))#Plots multiple time series on same plot. lty is line type 1 for first time series. 3 for second time series
# #---measure errors
# p1 = exp(trPred$pred[1:(shortPredAhead*timePer)])
# p2 = te[1:(shortPredAhead*timePer)]
# er = p1 - p2
# plot(er)
# meanSquaredError(p2, p1)


# testLim = 40
# predAhead = 20
# plot(d, main="Main plot", xlab="years", ylab="temperature")
# abline(reg=lm(d~time(d)))
# message("Type:",class(d))
# message("Starts at:",start(d))
# message("Ends at:",end(d))
# message("Frequency:",frequency(d))
# print(summary(d))#cycle(d)
# plot(aggregate(d,FUN=mean))#shows yearly trend
# boxplot(d~cycle(d))#plot across months. Shows seasonality
# plot(log(d), main="log(d) removes unequal variances", xlab="years", ylab="temperature")
# plot(diff(log(d)), main="diff(log(d) de-trends", xlab="years", ylab="temperature")
# #plot(log(diff(d)), main="As log(diff(d))", xlab="years", ylab="temperature")
# adft = adf.test(diff(log(d)), alternative="stationary", k=0)
# acf(diff(log(d)))
# pacf(diff(log(d)))
# fit <- arima(log(d), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
# pred <- predict(fit, n.ahead = 15*12)
# ts.plot(d,exp(pred$pred), log = "y", lty = c(1,3), col=c("blue","red"))#Plots multiple time series on same plot. lty is line type 1 for first time series. 3 for second time series
#
# #---dividing into test and training sets
# tr = window(d, start=1949, end=c(1955,12))
# te = window(d, start=1956, end=c(1960,12))
# #---checking for stationarity and predict
# acf(diff(log(d)))
# pacf(diff(log(d)))
# fitTr <- arima(log(tr), c(1,1,2),seasonal=list(order=c(1,1,2),period=12))
# trPred <- predict(fitTr, n.ahead=5*12)
# ts.plot(d,tr,exp(trPred$pred), log = "y", lty = c(3,3,3), col=c("green","blue","red"))#Plots multiple time series on same plot. lty is line type 1 for first time series. 3 for second time series
# #---measure errors
# p1 = exp(trPred$pred[1:(5*12)])
# p2 = te[1:(5*12)]
# er = p1 - p2
# plot(er)
# meanSquaredError(p2, p1)


meanSquaredError <- function(original, predicted) {
    original = na.trim(original)
    predicted = na.trim(predicted)
    m = sqrt((original - predicted)^2)
    return (m)
}

# A function to do exponential smoothing:
exponential.smooth <- function(x, lambda)
{
    if(length(lambda) > 1)
        stop("lambda must be a single number")
    if(lambda > 1 || lambda <= 0)
        stop("lambda must be between zero and one")
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
    acf(d)
    pacf(d)
}


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
