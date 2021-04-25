# Time series of gold prices

gold <- read.csv("C:/Users/shubhamv/Downloads/monthly_csv.csv")
#View(gold)
str(gold)
head(gold)
tail(gold)
summary(gold)

goldseries <- ts(gold$Price,start = c(1950,1),end = c(2020,7),frequency = 12)
goldseries
plot.ts(goldseries,col = 'purple')

# decomposition
goldseriescomponents <- decompose(goldseries)
plot(goldseriescomponents)

# There is an upward trend , seasonality exist , then there is random plot which
# we get when we remove trend and seasonality

goldseriesforecast <- HoltWinters(goldseries)
goldseriesforecast

# By default, HoltWinters() just makes forecasts for the same time period covered by our original time series.

goldseriesforecast$fitted # predicted values
plot(goldseriesforecast) # We can plot the original time series against the forecasts by typing:
goldseriesforecast$SSE # SSE : 521123.2
521123.2/847 # MSE
sqrt(615.2576)
# RMSE : 24.80439
library(forecast)
goldseriesforecast1 <- forecast:::forecast.HoltWinters(goldseriesforecast,h = 48) 
accuracy(goldseriesforecast1) # RMSE : 24.98199

# For example, the forecasted gold price for jul 2024 is about 2959.281, with a
# 95% prediction interval of (2126.343, 3792.220).

plot(goldseriesforecast1)

acf(goldseriesforecast1$residuals,lag.max = 20,na.action = na.pass)
# na.action function to be called to handle missing values. na.pass can be used.

# You can see from the sample correlogram that the autocorrelation at some lags  is just crossing the significance bounds. To
# test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test.

Box.test(goldseriesforecast1$residuals,lag = 20,type = 'Ljung-Box')
# Ho: there are no non-zero autocorrelations at lags 1-20
# null hypo is rejected as pvalue - 0.01625 less than 0.05

# the p-value is 0.01625, indicating that there is little evidence of non-zero autocorrelations 
# in the in-sample forecast errors at lags 1-20. ie it requires forecast errors are uncorrelated
# for if we want to make prediction intervals


# To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors
# are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant
# variance, we can make a time plot of the in-sample forecast errors:
plot.ts(goldseriesforecast1$residuals)

# no constant variance hence the model can be improved 

#install.packages("psych")
library(psych)
describe(goldseriesforecast1$residuals)
hist(goldseriesforecast1$residuals)

#  ARIMA Models
# Exponential smoothing methods are useful for making forecasts, and make no assumptions about the correlations
# between successive values of the time series. However, if you want to make prediction intervals for forecasts made
# using exponential smoothing methods, the prediction intervals require that the forecast errors are uncorrelated and are
# normally distributed with mean zero and constant variance.
# While exponential smoothing methods do not make any assumptions about correlations between successive values of
# the time series, in some cases you can make a better predictive model by taking correlations in the data into account.

# stationarity means series without trend and seasonality. 
plot.ts(goldseries)
goldseriesdiff <- diff(goldseries,differences = 1)
plot.ts(goldseriesdiff)

# we have removed the trend component of the time series , 
# and are left with an irregular component. We can now examine whether there are 
# correlations between successive terms of this irregular component; if so, this 
# could help us to make a predictive model.

# if your time series is stationary, or if you have transformed it to a stationary time series by differencing d times, the
# next step is to select the appropriate ARIMA model, which means finding the values of most appropriate values of p
# and q for an ARIMA(p,d,q) model. To do this, you usually need to examine the correlogram and partial correlogram
# of the stationary time series

acf(goldseriesdiff,lag.max = 20,col = 'red',
    lwd = 4)

acf(goldseriesdiff,lag.max = 20,
    plot = FALSE)

pacf(goldseriesdiff,lag.max = 20,
     col = 'green', lwd = 4)
pacf(goldseriesdiff,lag.max = 20,
     plot = FALSE)

# Stationarity check : ADF test

#install.packages('tseries')
library('tseries')
adf.test(goldseries)
#H0: non-stationary

adf.test(goldseriesdiff)
#H0: non-stationary

#install.packages('forecast')
library('forecast')
auto.arima(goldseries)

# model building (ARIMA)

goldseriesArima <- arima(goldseries,order = c(0,2,1)) 
# fit an ARIMA 0,2,1 model 
# autoregression(p=0) , diff(d=2),moving avg(q=1)

goldseriesArima # keep an eye on aic lowest is the best 

# Forecast

goldseriesArimaforecasts <- forecast(goldseriesArima, h=24) 
accuracy(goldseriesArimaforecasts)# RMSE : 24.80487

plot(goldseriesArimaforecasts,lwd = 2)

# As in the case of exponential smoothing models, it is a good idea to investigate whether 
# the forecast errors of an ARIMA model are normally distributed with mean zero and 
# constant variance, and whether the are correlations between successive forecast errors.

# For example, we can make a correlogram of the forecast errors for our ARIMA(0,2,1) model
# and perform the Ljung-Box test for lags 1-20, by typing:

acf(goldseriesArimaforecasts$residuals,lag.max = 20)
Box.test(goldseriesArimaforecasts$residuals,lag = 20,type = 'Ljung-Box')

# There is non-zero autocorr .

# To investigate whether the forecast errors are normally distributed with mean zero 
# and constant variance, we can make a time plot and histogram (with overlaid normal curve) 
# of the forecast errors:

plot.ts(goldseriesArimaforecasts$residuals) # make timeplot of residuals
hist(goldseriesArimaforecasts$residuals)

# Since successive forecast errors seems to be correlated, and the forecast errors doesn't seem to be 
# normally distributed with mean zero and constant variance, the ARIMA(0,1,1) doesn't seem to 
# provide an adequate predictive model for the gold prices.
