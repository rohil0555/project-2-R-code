library(fpp3)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)


library(stringr)
library(knitr)
library(seasonal)

library(readr)
library(plotly)
library(e1071)
library(caret)
library(tsibble)
library(fpp3)
library(readr)
library(forecast)




data<-read_csv("datasets_forecasting/CPIAUCSL.csv")
data

class(data)

#end(ts_data)
#frequency(ts_data)



#time series plot
ts_data <- ts(data$CPIAUCSL , start = c(1947), frequency = 12)
ts_data
plot(ts_data)
class(ts_data)

#acf plot

acf(ts_data, lag.max=150)

#data is strongly  positively correlated 






#decomposition
dts_data <- decompose(ts_data, "multiplicative")
plot(dts_data)
plot(dts_data$trend)
plot(dts_data$seasonal)
plot(dts_data$random)

#spliting the data in train and test
train = data[1:828,]
test = data[829:912,]
train <- ts(train$CPIAUCSL , start = c(1947), frequency = 12)
test <- ts(test$CPIAUCSL , start = c(2016), frequency = 12)

data[828,1]

mymodel <- auto.arima(train)
mymodel

plot.ts(mymodel$residuals)

#Forcasting for next 5 years
myforecast <- forecast(test, level=c(95), h=7)
plot(myforecast)


#Fit STL-based forecasting model to data
model <- stlf(ts_data, s.window = "periodic")

# Plot forecast and actual values
autoplot(model) + xlab("Year") + ylab("CPI") + ggtitle("INFLATION") +
  autolayer(ts_data, series = "Actual") + 
  guides(colour = guide_legend(title = "Forecast"))



















































