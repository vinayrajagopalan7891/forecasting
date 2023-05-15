library(fpp2)
library(tidyverse)
library(readxl)
library(portes)

# Read the data
setwd("C:/Users/vrajagopalan/OneDrive - IESEG/Desktop/Forcasting") # Specify you own working directory here.
data <- read_excel("DataSets2022.xlsx", sheet="Airpass_BE")
head(data)

# Initialize Timeseries
air_ts <- ts(data[,2], frequency = 12, start = 2003)

#Initialize Train Test Validation Dataset
final_air_ts = window(air_ts, start = c(2003, 1),  end = c(2020,2))
# Train January 2003 up to December 2017
train = window(air_ts, start = c(2003, 1), end=c(2017,12))
train
# Test January 2018 up to February 2020 
test = window(air_ts, start=c(2018,1), end = c(2020,2))
test
#Validation March 2020 - October 2021
validation = window(air_ts, start=c(2020,3), end = c(2021,10))
validation



#Question1
#Plot Timeseries
plot(air_ts)
tsdisplay(air_ts)
ggseasonplot(air_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Air Passengers") +
  ggtitle("Seasonal plot:  International intra-EU air passenger")
ggsubseriesplot(air_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Air Passengers") +
  ggtitle("Month plot:  International intra-EU air passenger")

#Question 2
final_air_ts
plot(final_air_ts)
tsdisplay(final_air_ts)

#Applying log tranformation
l_final_air_ts <- log(final_air_ts)
plot(l_final_air_ts)
tsdisplay(l_final_air_ts)
l_final_air_ts
plot(l_final_air_ts)
tsdisplay(diff(l_final_air_ts,12))

BoxCox.lambda(final_air_ts)

df <- data.frame()
# Question 3
h = length(test)
f1 <- snaive(train, h=h, lambda = 0)
checkresiduals(f1)
plot(f1)
checkresiduals(f1)
accuracy(f1,test)[,c(2,3,5,6)]

df <- rbind(df, cbind('snaive', accuracy(f1,test)[,c(2,3,5,6)]))
df

# Question 4
# Using stl
# Decompose the time series
dim(train) <- NULL
fit <- stl(log(train), s.window="periodic", t.window = 12)
fc <- forecast(fit, method="naive", h=h)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- train
# Plot result on original scale
plot(fc, ylim = c(50000,3000000))
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlnaive', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df

fit <- stl(log(train), s.window="periodic", t.window = 12)
fcast <- forecast(fit, method="rwdrift", h=h)
plot(fcast)
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlrwdrift', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df

fit <- stl(log(train), s.window="periodic", t.window = 12)
fcast <- forecast(fit, method="ets", h=h)
plot(fcast)
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlets', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df

fit <- stl(log(train), s.window="periodic", t.window = 12)
fcast <- forecast(fit, method="arima", h=h)
plot(fcast)
checkresiduals(fcast)
accuracy(fcast,log(test))[,c(2,3,5,6)]
df <- rbind(df, cbind('stlarima', accuracy(fcast,log(test))[,c(2,3,5,6)]))
df


# Question 5
fit1 <- ets(log(train), model="AAA" , restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 26)

fit1 <- ets(log(train), model="ANA" , restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 26)


fit1 <- ets(log(train), model="MAM", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1,lag = 26)

fit1 <- ets(log(train), model="MNM", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1,lag = 26)

fit1 <- ets(log(train), model="MAAd", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 26)

fit1 <- ets(log(train), model="MAA", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 26)

fit1 <- ets(log(train), model="MNA", restrict = TRUE)
accuracy(forecast(fit1,h),log(test))[,c(2,3,5,6)]
checkresiduals(fit1, lag = 26)

fit <- ets(log(train), restrict = TRUE)
summary(fit)
accuracy(forecast(fit,h),log(test))[,c(2,3,5,6)]
fc <- forecast(fit,h)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- train
# Plot result on original scale
plot(fc, ylim = c(50000,3000000))
checkresiduals(fit, lag = 26)
res <- residuals(fit)
tsdisplay(res)



# Question 6
#Checking for applicable diffs.
nsdiffs(l_final_air_ts)
d_l_final_air_ts = diff(l_final_air_ts,12)
ndiffs(d_l_final_air_ts)
nsdiffs(d_l_final_air_ts)
tsdisplay(d_l_final_air_ts)
tsdisplay(final_air_ts)
#adf.test(d_l_final_air_ts)

a <- auto.arima(log(train), allowdrift = FALSE,
                approximation = FALSE, stepwise = FALSE,)
summary(a)
fa <- forecast(a, h=h)
accuracy(fa, log(test))[,c(2,3,5,6)]
checkresiduals(a)
tsdisplay(a$residuals)

a <- auto.arima(log(train), allowdrift = FALSE,
                approximation = FALSE, stepwise = FALSE,d= 1, D=1)
summary(a)
fa <- forecast(a, h=h)
accuracy(fa, log(test))[,c(2,3,5,6)]
checkresiduals(a)
tsdisplay(a$residuals)

fcast = Arima(log(train),
              order = c(1,0,1), seasonal = c(5,1,0), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast)
fc <- forecast(fcast,h)
fc
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- train
# Plot result on original scale
plot(fc, ylim = c(50000,3000000))
res <- residuals(fcast)
tsdisplay(res)


fcast = Arima(log(train),
              order = c(1,1,0), seasonal = c(5,1,1), include.drift = FALSE)
fcast
accuracy(forecast(fcast, h = h), log(test))[,c(2,3,5,6)]
checkresiduals(fcast)
res <- residuals(fcast)
tsdisplay(res)
         
#Question 8 
fit <- ets(log(final_air_ts), model="AAA" , restrict = TRUE)
fc <- forecast(fit, h = 34)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
fc$mean <- exp(fc$mean)
fc$x <- final_air_ts
# Plot result on original scale
plot(fc, ylim = c(50000,3000000))

#Question 9
plot(air_ts, ylim = c(50000,3000000))
lines(fc$mean, col = 'red')
legend("bottomleft",lty=1, cex=0.5,col='red',legend='ETS(AAA) Forecast')
