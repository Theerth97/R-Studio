library(forecast) #Time series forecast and modeling
library(tseries) # to read time series and ts plots
library(readxl) # To read excel files
library(TSstudio) # superb visualizations
View(Gold)
gts=ts(Gold[,2],frequency = 12,start = c(1969,12),end = c(2018,7))
class(gts)
View(gts)
plot(gts)
abline(reg = lm(gts~time(gts)))

cycle(gts)
plot(aggregate(gts, FUN = mean))
plot(GTS)
#Visualization
library(TSstudio)
ts_plot(gts,title = "Gold prices in US Dollars per ounce", Xtitle = "Year", Ytitle = "Price per ounce")
ts_heatmap(gts)
ts_polar(gts)
ts_decompose(gts, type = "both")
ts_lags(gts)
ts_lags(GTS)
acf(gts, lag.max = 20)
pacf(gts, lag.max = 20)

#Test for Stationarity
library(tseries)
adf.test(gts, "stationary")
acf(gts, main="Auto Correlation Function")

D1<-diff(gts, differences = 1)
D1
ts_plot(D1, title = "Differenced time series plot")

adf.test(D1, "stationary")
acf(D1, main="Auto Correlation Function of Differenced series")
pacf(D1, main="Partial Auto Correlation Function of Differenced series")

library(TSstudio)

h <- 24

# Split the data into training and testing sets
#(leaving the last 24 months for testing)
split_GTS <- ts_split(gts, sample.out = h)

train <- split_GTS$train
test <- split_GTS$test

head(train, 5)
start(train); end(train) # start date and end date of training set

head(test, 5)
start(test); end(test) # start date and end date of training set
#Residuals analysis
#The check_res function visualize the traditional plot of the residuals over time,
#and the corresponding auto correlation function and the histogram (of the residuals):
library(forecast)
# Building a model on the training set
fit <- auto.arima(train, lambda = BoxCox.lambda(train))
fit
accuracy(fit)
# Checking the residuals
res<-check_res(fit)
res

#Forecast evaluation with the test_forecast
#The test_forecast function visualizes the fitted values vs the training
#partition values and the forecasted values vs the testing partitions values.
#In addition, the tooltip of the plot provides information about the model performance
#(MAPE and RMSE of the training and testing partitions):

fc <- forecast(fit, h = h)
fc
accuracy(fc)

tfc<-test_forecast(actual = gts, forecast.obj = fc, test = test)
tfc
























