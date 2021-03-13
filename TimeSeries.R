# Time series 

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip = 3)
kings

View(kings)
summary(kings)
hist(kings,col = "pink")
boxplot(kings,horizontal = TRUE,col = "purple")
par(mfrow = c(2,1))

# Once you have read the time series data into R, the next step is to store the data in a time series object in R, so that
# you can use R's many functions for analysing time series data. To store the data in a time series object, we use the ts()
# function in R.

kingtimeseries <- ts(kings)
kingtimeseries

# Sometimes the time series data set that you have may have been collected at regular intervals that were less than one
# year, for example, monthly or quarterly. In this case, you can specify the number of times that data was collected per
# year by using the 'frequency' parameter in the ts() function. For monthly time series data, you set frequency=12, while
# You can also specify the first year that the data was collected, and the first interval in that year by using the 'start'
# parameter in the ts() function. For example, if the first data point corresponds to the second quarter of 1986, you would
# set start=c(1986,2).

plot.ts(kingtimeseries, col = "red")

# We can see from the time plot that this time series could probably be described using an additive model, since the
# random fluctuations in the data are roughly constant in size over time.
#Trend, Cycle, Seasonality NOT EXISTING. Randomness is roughly constant!

# Decomposing Non-Seasonal Data
# A non-seasonal time series consists of a trend component and an irregular component. Decomposing the time series
# involves trying to separate the time series into these components, that is, estimating the the trend component and the
# irregular component.

# MOVING AVERAGE 
# To estimate the trend component of a non-seasonal time series that can be described using an additive model, it is
# common to use a smoothing method, such as calculating the simple moving average of the time series.

install.packages("TTR")
library(TTR)
kingtimeseriesSMA3 <- SMA(kingtimeseries,n=3)
plot.ts(kingtimeseriesSMA3, col = "red")

kingtimeseriesSMA8 <- SMA(kingtimeseries,n=8)
plot.ts(kingtimeseriesSMA8, col = "red")

# The data smoothed with a simple moving average of order 8 gives a clearer picture of the trend component, and we
# can see that the age of death of the English kings seems to have decreased from about 55 years old to about 38 years
# old during the reign of the first 20 kings, and then increased after that to about 73 years old by the end of the reign of
# the 40th king in the time series.

# Data : Birth 

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
birthstimeseries <- ts(births,frequency = 12,start = c(1946,1))
birthstimeseries
summary(birthstimeseries)
plot.ts(birthstimeseries)

# Peak in summers , trough in winters , seasonality exist 

#Decomposition time series 

timeseriescomponents <- decompose(birthstimeseries)
timeseriescomponents
plot(timeseriescomponents)

# Trend exists, No Cycle; Seasonality exists, randomness is roughly constant

timeseriescomponents$seasonal
birthstimeseriesseasonallyadjusted <- birthstimeseries - timeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
# No seasonality 

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir,frequency = 12,start=c(1987,1))
souvenirtimeseries

plot.ts(souvenirtimeseries,col = "black",lwd = 4)

# It looks like that information is hidden and we are not able to see

logsouvenirtimeseries <- log(souvenirtimeseries)
plot(logsouvenirtimeseries)

souvenirtimeseries    # look at the diff
logsouvenirtimeseries

# Forecasts using Exponential Smoothing
# Exponential smoothing can be used to make short-term forecasts for time series data.

#If you have a time series that can be described using an additive model with constant level and no seasonality, you can
#use simple exponential smoothing to make short-term forecasts.
#The simple exponential smoothing method provides a way of estimating the level at the current time point. Smoothing
#is controlled by the parameter alpha; for the estimate of the level at the current time point. The value of alpha; lies
#between 0 and 1. Values of alpha that are close to 0 mean that little weight is placed on the most recent observations
#when making forecasts of future values.

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip = 1)
rain
raintimeseries <- ts(rain,start = c(1813))
raintimeseries
plot.ts(raintimeseries,col = 'purple',lwd = 2)

# You can see from the plot that there is roughly constant level (the mean stays constant at about 25 inches).
#  The random fluctuations in the time series seem to be roughly constant in size over time, so it is probably appropriate to describe
# the data using an additive model. Thus, we can make forecasts using simple exponential smoothing.
# To make forecasts using simple exponential smoothing in R, we can fit a simple exponential smoothing predictive
# model using the "HoltWinters()" function in R. To use HoltWinters() for simple exponential smoothing, we need to
# set the parameters beta=FALSE and gamma=FALSE in the HoltWinters() function (the beta and gamma parameters
# are used for Holt's exponential smoothing, or Holt-Winters exponential smoothing, as described below).
# The HoltWinters() function returns a list variable, that contains several named elements.

raintimeseriesforecasts <- HoltWinters(raintimeseries,beta = FALSE,gamma = FALSE) 
raintimeseriesforecasts

# The output of HoltWinters() tells us that the estimated value of the alpha parameter is about 0.024. 
# This is very close to zero, telling us that the forecasts are based on both recent and 
# less recent observations (although somewhat more weight is placed on recent observations).

# By default, HoltWinters() just makes forecasts for the same time period covered by our original time series. In this
# case, our original time series included rainfall for London from 1813-1912, so the forecasts are also 
# for 1813-1912.
# In the example above, we have stored the output of the HoltWinters() function in the 
# list variable "rainseriesforecasts".
# The forecasts made by HoltWinters() are stored in a named element of this list variable called "fitted", 
# so we can get by typing 

raintimeseriesforecasts$fitted

# We can plot the original time series against the forecasts by typing:
  
plot(raintimeseriesforecasts)

# The plot shows the original time series in black, and the forecasts as a red line. The time series of forecasts is much
# smoother than the time series of the original data here.
# As a measure of the accuracy of the forecasts, we can calculate the sum of squared errors for the 
# in-sample forecast
# errors, that is, the forecast errors for the time period covered by our original time series. 
# The sum-of-squared-errors is stored in a named element of the list 
# variable "rainseriesforecasts" called "SSE", so we can get its value by typing:

raintimeseriesforecasts$SSE # [1] 1828.855

# It is common in simple exponential smoothing to use the first value in the time series as the 
# initial value for the level.
# For example, in the time series for rainfall in London, the first 
# value is 23.56 (inches) for rainfall in 1813. You can
# specify the initial value for the level in the HoltWinters() function by using the "l.start" parameter. 
# For example, to make forecasts with the initial value of the level set to 23.56, we type:
  
HoltWinters(raintimeseries, beta=FALSE, gamma=FALSE, l.start=23.56)

# As explained above, by default HoltWinters() just makes forecasts for the time period covered 
# by the original data, which is 1813-1912 for the rainfall time series. 
#We can make forecasts for further time points by using 
# the "forecast.HoltWinters()" function in the R "forecast" package. 

#install.packages("forecast")
library(forecast)

# You specify how many further time
# points you want to make forecasts for by using the "h" parameter in forecast.HoltWinters().

raintimeseriesforecasts2 <- forecast:::forecast.HoltWinters(raintimeseriesforecasts,h=8)
raintimeseriesforecasts2

# The forecast.HoltWinters() function gives you the forecast for a year, a 80% prediction interval 
# for the forecast, and a 95% prediction interval for the forecast. For example, the 
# forecasted rainfall for 1920 is about 24.68 inches, with a
# 95% prediction interval of (16.24, 33.11).

forecast:::plot.forecast(raintimeseriesforecasts2,col = 'purple',lwd = 4)

#Here the forecasts for 1913-1920 are plotted as a blue line, the 80% prediction 
#interval as an orange shaded area, and
#the 95% prediction interval as a yellow shaded area.
#The 'forecast errors' are calculated as the observed values minus predicted values, 
#for each time point. We can only
#calculate the forecast errors for the time period covered by our original 
#time series, which is 1813-1912 for the rainfall
#data. As mentioned above, one measure of the accuracy of the predictive model 
#is the sum-of-squared-errors (SSE)
#for the in-sample forecast errors.
#The in-sample forecast errors are stored in the named element "residuals" of the list variable
#returned by forecast.HoltWinters(). If the predictive model cannot be improved upon, 
#there should be no correlations between forecast
#errors for successive predictions. In other words, if there are correlations between forecast errors 
#for successive predictions, it is likely that the simple exponential smoothing forecasts 
#could be improved upon by another forecasting technique.
# To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20. We
#can calculate a correlogram of the forecast errors using the "acf()" function in R. 
# To specify the maximum lag that we
#want to look at, we use the "lag.max" parameter in acf().

raintimeseriesforecasts2$residuals
raintimeseriesforecasts2$fitted
raintimeseries
# 26.07 - 23.56 = 2.51

acf(raintimeseriesforecasts2$residuals,lag.max = 10,na.action = na.pass)

#You can see from the sample correlogram that the autocorrelation at lag 3 is just touching the significance bounds. To
#test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test.
#This can be done in R using the "Box.test()", function. The maximum lag that we want to look at is specified using#
#the "lag" parameter in the Box.test() function. For example, to test whether there are non-zero autocorrelations at lags
#1-20, for the in-sample forecast errors for London rainfall data, we type:

Box.test(raintimeseriesforecasts2$residuals,lag = 20, type = 'Ljung-Box') # H0: There is no significant auto correlation.

# Box-Ljung test

# data:  raintimeseriesforecasts2$residuals
# X-squared = 17.401, df = 20, p-value = 0.6268
# P-value 0.6 , accepted.

# Here the Ljung-Box test statistic is 17.4, and the p-value is 0.6, so there is little evidence of non-zero autocorrelations
# in the in-sample forecast errors at lags 1-20.
# To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors
# are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant
# variance, we can make a time plot of the in-sample forecast errors:
  
plot.ts(raintimeseriesforecasts2$residuals, col = 'purple',lwd = 4)

# The plot shows that the in-sample forecast errors seem to have roughly constant variance over time, although the size of
# the fluctuations in the start of the time series (1820-1830) may be slightly less than that at later dates (eg. 1840-1850).

# To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast
# errors, with an overlaid normal curve that has mean zero and the same 
# standard deviation as the distribution of forecast errors. 

library(psych)
describe(raintimeseriesforecasts2$residuals)

hist(raintimeseriesforecasts2$residuals,col = 'blue')

# As it is approximately Normally Distributed, FURTHER improvement in model is not needed

# Holt's Exponential Smoothing
# If you have a time series that can be described using an additive model with increasing or decreasing trend and no
# seasonality, you can use Holt's exponential smoothing to make short-term forecasts.


skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirttimeseries <- ts(skirts,start = c(1866))
plot.ts(skirttimeseries)

# We can see from the plot that there was an increase in hem diameter from about 600 in 1866 to about 1050 in 1880,
# and that afterwards the hem diameter decreased to about 520 in 1911.

# To make forecasts, we can fit a predictive model using the HoltWinters() function in R. To use HoltWinters() for Holt's
# exponential smoothing, we need to set the parameter gamma=FALSE (the gamma parameter is used for Holt-Winters
                                                                 # exponential smoothing, as described below).

skirttimeseriesforecast <- HoltWinters(skirttimeseries,gamma = FALSE)
skirttimeseriesforecast

# Holt-Winters exponential smoothing with trend and without seasonal component.

# Call:
#  HoltWinters(x = skirttimeseries, gamma = FALSE)

# Smoothing parameters:
#  alpha: 0.8383481
# beta : 1
# gamma: FALSE

#Coefficients:
# [,1]
# a 529.308585
# b   5.690464

skirttimeseriesforecast$SSE # 16954.18

# The estimated value of alpha is 0.84, and of beta is 1.00. These are both high, telling us that both the estimate of the
# current value of the level, and of the slope b of the trend component, are based mostly upon very recent observations
# in the time series. This makes good intuitive sense, since the level and the slope of the time series both change quite a
# lot over time. The value of the sum-of-squared-errors for the in-sample forecast errors is 16954.

# We can plot the original time series as a black line, with the 
# forecasted values as a red line on top of that, by typing:

plot(skirttimeseriesforecast)

# We can see from the picture that the in-sample forecasts agree pretty well with the observed values, although they tend
#to lag behind the observed values a little bit.
#If you wish, you can specify the initial values of the level and the slope b of the trend component by using the "l.start"
#and "b.start" arguments for the HoltWinters() function. It is common to set the initial value of the level to the first value
#in the time series (608 for the skirts data), and the initial value of the slope to the second value minus the first value
#(9 for the skirts data). For example, to fit a predictive model to the skirt hem data using Holt's exponential smoothing,
#with initial values of 608 for the level and 9 for the slope b of the trend component, we type:
  
HoltWinters(skirttimeseries,gamma = FALSE,l.start = 608,b.start = 9)

# As for simple exponential smoothing, we can make forecasts for future times not covered by the original time series by
#using the forecast.HoltWinters() function in the "forecast" package. For example, our time series data for skirt hems
#was for 1866 to 1911, so we can make predictions for 1912 to 1930 (19 more data points), and plot them, by typing:

skirttimeseriesforecast2 <- forecast:::forecast.HoltWinters(skirttimeseriesforecast,h=19)
skirttimeseriesforecast2

forecast:::plot.forecast(skirttimeseriesforecast2)

#The forecasts are shown as a blue line, with the 80% prediction intervals as an orange shaded area, and the 95%
#prediction intervals as a yellow shaded area.
#As for simple exponential smoothing, we can check whether the predictive model could be improved upon by checking
#whether the in-sample forecast errors show non-zero autocorrelations at lags 1-20. For example, for the skirt hem data,
#we can make a correlogram, and carry out the Ljung-Box test, by typing:
 
acf(skirttimeseriesforecast2$residuals,lag.max = 20,na.action = na.pass)

#1 out of 20 has crossed limits! No worries as we are working with 5% LOS

Box.test(skirttimeseriesforecast2$residuals,lag = 20,type = 'Ljung-Box')
# H0 : There are no non-zero autocorr 
# pvalue : 0.4749 , Accepted 

# NOTE: if there is corr then improvment needed if no corr no improvment needed.

#  Indeed, when we carry out the Ljung-Box test, the p-value is 0.47,
#indicating that there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.
#As for simple exponential smoothing, we should also check that the forecast errors have constant variance over time,
#and are normally distributed with mean zero. We can do this by making a time plot of forecast errors, and a histogram
#of the distribution of forecast errors with an overlaid normal curve:
 
plot.ts(skirttimeseriesforecast2$residuals) # make a time plot
hist(skirttimeseriesforecast2$residuals, col = "green")
library(psych)
describe(skirttimeseriesforecast2$residuals)  


# Autoregressive Integrated Moving Average (ARIMA) models include an explicit statistical model for the irregular
# component of a time series, that allows for non-zero autocorrelations in the irregular component.

# Differencing a Time Series

# ARIMA models are defined for stationary time series. Therefore, if you start off with a non-stationary time series, you
# will first need to 'difference' the time series until you obtain a stationary time series. If you have to difference the time
# series d times to obtain a stationary series, then you have an ARIMA(p,d,q) model, where d is the order of differencing
# used.

# Stationarity: Series without trend and seasonality

skirttimeseriesdiff1 <- diff(skirttimeseries,differences = 1)
plot.ts(skirttimeseriesdiff1)
plot.ts(skirttimeseries)
par(mfrow = c(2,1))

# The resulting time series of first differences (above) does not appear to be stationary in mean. Therefore, we can
# difference the time series twice, to see if that gives us a stationary time series:
  

skirttimeseriesdiff2 <- diff(skirttimeseries,differences = 2)
plot.ts(skirttimeseriesdiff2)

plot.ts(kingtimeseries)
# From the time plot (above), we can see that the time series is not stationary in mean. To calculate the time series of
# first differences, and plot it, we type   

kingtimeseriesdiff1 <- diff(kingtimeseries,differences = 1)
plot.ts(kingtimeseriesdiff1)

# The time series of first differences appears to be stationary in mean and variance, and so an ARIMA(p,1,q) model is
# probably appropriate for the time series of the age of death of the kings of England. By taking the time series of first
# differences, we have removed the trend component of the time series of the ages at death of the kings, and are left
# with an irregular component. We can now examine whether there are correlations between successive terms of this
# irregular component; if so, this could help us to make a predictive model for the ages at death of the kings.

# Selecting a Candidate ARIMA Model

# If your time series is stationary, or if you have transformed it to a stationary time series by differencing d times, the
# next step is to select the appropriate ARIMA model, which means finding the values of most appropriate values of p
# and q for an ARIMA(p,d,q) model. To do this, you usually need to examine the correlogram and partial correlogram
# of the stationary time series.

# To plot a correlogram and partial correlogram, we can use the "acf()" and "pacf()" functions in R, respectively. To get
# the actual values of the autocorrelations and partial autocorrelations, we set "plot=FALSE" in the "acf()" and "pacf()"
# functions.

acf(kingtimeseriesdiff1,lag.max = 20) # plot a correlogram
acf(kingtimeseriesdiff1,lag.max = 20,plot = FALSE) # get the autocorrelation value

# We see from the correlogram that the autocorrelation at lag 1 (-0.360) exceeds the significance bounds, but all other
# autocorrelations between lags 1-20 do not exceed the significance bounds.

# To plot the partial correlogram for lags 1-20 for the once differenced time series of the ages at death of the English
# kings, and get the values of the partial autocorrelations, we use the "pacf()" function, by typing:

pacf(kingtimeseriesdiff1,lag.max = 20) # plot a partial correlogram 
pacf(kingtimeseriesdiff1,lag.max = 20,plot = FALSE) # get the partial autocorrelation values

# Stationarity check : ADF test

library("tseries")
adf.test(kingtimeseries)
adf.test(kingtimeseriesdiff1)

# Auto regressive Integrated moving average

auto.arima(kingtimeseries)
kingtimeseriesARIMA <- arima(kingtimeseries,order = c(1,1,2))
kingtimeseriesARIMA

# Keep an eye on aic [lowest is the best!]
library("forecast")
fcastkings <- forecast(kingtimeseriesARIMA,h=5)
plot(fcastkings)

# ARIMA acf and pcf

tsdisplay(residuals(kingtimeseriesARIMA),
          lag.max = 20,
          main = "(1,1,2) Model Residuals",
          col = 'purple',lwd = 4)
# stand alone line diagram of residuals

plot (kingtimeseriesARIMA$residuals,
     lwd = 3, col = 'green',
     main = 'Residuals plot (1,1,2')

# histogram of residuals 

hist(kingtimeseriesARIMA$residuals,
     col = 'green',
     main = 'Histogram of residuals(1,1,2)')

describe(kingtimeseriesARIMA$residuals)

# lets confirm statistically 

Box.test(kingtimeseriesARIMA$residuals,
         lag = 20,
         type = 'Ljung-Box')

# ACid test : Accuracy 

accuracy(kingtimeseriesARIMA) # RMSE : 14.89396
















