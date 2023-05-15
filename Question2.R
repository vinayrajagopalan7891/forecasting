library(fpp2)
library(tidyverse)
library(readxl)
library(ggfortify)

setwd('C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Forcasting')
data <- read_excel("ND000349Q.xls")

# Initialize Timeseries
consumption_expenditure <- ts(data[,2], frequency = 4, start = c(2002,1))
consumption_expenditure

#Initialize Train Test Validation Dataset
final_consumption_expenditure = window(consumption_expenditure, start = c(2002, 1),  end = c(2019,4))
final_consumption_expenditure
# Train Q1 2003 up to Q4 2017
train = window(consumption_expenditure, start = c(2003, 1), end=c(2017,4))
train
# Test Q1 2018 up to Q4 2019 
test = window(consumption_expenditure, start=c(2018,1), end = c(2019,4))
test
#Validation Q1 2020 - Q4 2021
validation = window(consumption_expenditure, start=c(2020,1), end = c(2021,4))
validation

#Plot Timeseries
plot(consumption_expenditure)
tsdisplay(consumption_expenditure)
ggseasonplot(consumption_expenditure, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("consumption_expenditure") +
  ggtitle("Seasonal plot:  Consumption Expenditure")
ggsubseriesplot(consumption_expenditure, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Air Passengers") +
  ggtitle("Month plot:  Consumption Expenditure")


consumption_expenditure
plot(consumption_expenditure)
tsdisplay(consumption_expenditure)

#Applying log tranformation
l_consumption_expenditure <- log(consumption_expenditure)
plot(l_consumption_expenditure)
tsdisplay(l_consumption_expenditure)
l_consumption_expenditure
plot(l_consumption_expenditure)
tsdisplay(diff(l_consumption_expenditure,12))

BoxCox.lambda(final_consumption_expenditure)

df <- data.frame()
h = length(test)


# Using stl
# Decompose the time series
dim(train) <- NULL
fit <- stl(log(train), s.window="periodic")
fcast <- forecast(fit, method="naive", h=h)
plot(fcast)
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlnaive', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df

fit <- stl(log(train), s.window="periodic")
fcast <- forecast(fit, method="rwdrift", h=h)
plot(fcast)
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlrwdrift', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df

fit <- stl(log(train), s.window="periodic")
fcast <- forecast(fit, method="ets", h=h)
plot(fcast)
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlets', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df

fit <- stl(log(train), s.window="periodic")
fc <- forecast(fit, method="arima", h=h)
checkresiduals(fc)
accuracy(fc,log(test))[,c(2,3,5,6)]
plot(fc)
# Plot result on original scale
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- train
plot(fc, ylim=c(2100,3500))
df <- rbind(df, cbind('stlarima', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df


# ETS MODELS
fit1 <- ets(log(train), model="AAA" , restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 15)


fit1 <- ets(log(train), model="MAM", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 15)
# Plot result on original scale
fc <- forecast(fit1,h)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- train
plot(fc, ylim=c(2100,3500))


fit1 <- ets(log(train), model="MAA", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 15)


# ARIMA MODELS
#Checking for applicable diffs.
nsdiffs(l_consumption_expenditure)
d_l_consumption_expenditure = diff(l_consumption_expenditure,12)
ndiffs(d_l_consumption_expenditure)
nsdiffs(d_l_consumption_expenditure)
tsdisplay(d_l_consumption_expenditure)
tsdisplay(consumption_expenditure)


fcast = Arima(log(train),
              order = c(1,0,0), seasonal = c(1,1,0), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast, lag = 16)
plot(forecast(fcast, h = h))
res <- residuals(fcast)
tsdisplay(res)
# Plot result on original scale
fc <- forecast(fcast,h)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- train
plot(fc, ylim=c(2100,3500))


fcast = Arima(log(train),
              order = c(1,0,1), seasonal = c(1,1,0), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast)
res <- residuals(fcast)
tsdisplay(res)

fcast = Arima(log(train),
              order = c(2,0,0), seasonal = c(1,1,0), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast)
res <- residuals(fcast)
tsdisplay(res)

fcast = Arima(log(train),
              order = c(1,0,1), seasonal = c(1,1,1), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast)
res <- residuals(fcast)
tsdisplay(res)

fcast = Arima(log(train),
              order = c(2,0,1), seasonal = c(1,1,1), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast)
res <- residuals(fcast)
tsdisplay(res)

#Question 8 
fit <- ets(log(consumption_expenditure), model="AAA" , restrict = TRUE)
result <- forecast(fit, h = 34)
plot(result)

#Question 9
fcast = Arima(log(final_consumption_expenditure),
              order = c(1,0,0), seasonal = c(1,1,0), include.drift = FALSE)
fc <- forecast(fcast, h = length(validation))
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- final_consumption_expenditure
plot(consumption_expenditure, ylim= c(1000, 4000))
lines(fc$mean, col = 'red')
legend("bottomleft",lty=1, cex=0.5,col='red',legend='Arima(1,0,0)(1,1,0)[4] Forecast')


