suppressWarnings({})
df=read.csv('E:\\Main project Journals\\Unemployment_rate_in_kerala\\df - Copy.csv')
# Load required libraries
library(forecast)

# Create a time series object
myts <- ts(data = df$Value, frequency = 30)

# Apply Box-Cox transformation
lambda <- BoxCox.lambda(myts)
myts_bc <- BoxCox(myts, lambda)

# Plot original and transformed time series
par(mfrow = c(2, 1))
plot(myts, main = "Original Time Series")
plot(myts_bc, main = "Transformed Time Series")

library(TTR)
library(forecast)
library(tseries)
library(moments)
attach(df)
summary(Value)
sd(Value)
var(Value)
skewness(Value)
kurtosis(Value)
plot.ts(Value)
adf.test(Value)
#pp.test(Value)
kpss.test(Value)
acf(Value,main='Autocorrelation plot of Unemployment rate in India')
pacf(Value,main='Partial autocorrelation plot of Unemployment rate in India')

#SMA
sma=SMA(remove,30)
sma
forecasteddf=forecast(sma,30)
forecasteddf
forecasteddf$model
accuracy(forecasteddf)
plot(forecasteddf,xlab = "Time",ylab = "Unemployment rate ",main = "Forecast from Simple Moving Average")
autoplot(forecasteddf,xlab = "Time",ylab = "Unemployment Rate ",main = "Forecast from Simple Moving Average",ts.colour = 'blue')
library(ggplot2)
#ARIMA
arima=auto.arima(df$Value,ic="aic",trace=TRUE,approximation=FALSE,stepwise=FALSE)
arima
#auto.arima(Value)
model=arima(Value,order = c(3,1,1))
model
forecast=forecast(model,h=30)
forecast
plot(forecast)
accuracy(forecast)
##Diagnostic
et=residuals(model)
acf(et)
plot.ts(et)
gghistogram(et)
Box.test(et,lag = 10,type = c("Box-Pierce","Ljung-Box"), fitdf = 4)
#Exponential smoothing
ses=ses(Value,h=30)
ses
ses$model
autoplot(ses)
accuracy(ses)

#Diagnostic checking of SMA
actual=Value[30:2115]
et_SMA= actual-forecasteddf$fitted
et_SMA=forecasteddf$residuals
write.csv(et_SMA,file = "C:\\Users\\Maneesha\\Documents\\SMA.csv")
getwd()
#et_SMA=residuals(forecasteddf$model)
sd_res <- sd(et_SMA, na.rm = TRUE)
std_residuals <- et_SMA / sd_res
std_residuals
acf(et_SMA)
pacf(et_SMA)
plot.ts(et_SMA)
gghistogram(et_SMA)
qqnorm(et_SMA, pch = 1, frame = FALSE)
qqline(et_SMA, col = "steelblue", lwd = 2)

shapiro.test(std_residuals)
ks.test(std_residuals, "pnorm")
fitted=forecasteddf$x
plot(fitted,std_residuals,xlab="fitted values",ylab="residuals",main="Residual vs fitted Graph")
plot(std_residuals,xlab="Time",ylab="residuals",main="Residual vs Time Graph")

acf(std_residuals)
pacf(std_residuals)
Box.test(std_residuals, lag = 13, type = "Ljung-Box")
jarque.bera.test(std_residuals)
#RMSE of Actual & forecasted
library(Metrics)
Jan=read.csv('E:\\Main project Journals\\Unemployment_rate_in_kerala\\2023 jan & feb.csv')
attach(Jan)
#SMA
library(Metrics)
RMSE_SMA=rmse(Jan$Value[1:7],forecasteddf$mean[1:7])
RMSE_SMA
#ARIMA
RMSE_ARIMA=rmse(Jan$Value,forecast$mean)
RMSE_ARIMA
#SES
RMSE_ES=rmse(Jan$Value,ses$mean)
RMSE_ES

# Load required libraries
library(tsoutliers)

# Create a time series object
myts <- ts(data = df, frequency = 30)

# Detect outliers using tsoutliers
outliers <- tso(myts, types = c("AO", "LS", "TC"), remove = FALSE)

# Plot the time series with outliers
plot(outliers)

library(car)
durbinWatsonTest(et_SMA)
library(caTools)
shapiro.test(resid(model))
s=durbin.watson(resid(model))
durbinWatsonTest(resid(model))
qqnorm(resid(model))
kpss.test(Value)
adf.test(Value)
s=log(diff(Value))
s
s=diff(log(Value))
s
adf.test(s)
d=auto.arima(s)
d
r=resid(d)
r
qqnorm(r)
durbinWatsonTest(d$residuals)
shapiro.test(d$residuals)
goldfeld

library(outliers)
v=grubbs.test(Value)
v
v=grubbs.test(Value,opposite = TRUE)
v
r=which.max(Value)
r
o=Value[-r]
o
d=dixon.test(o)
