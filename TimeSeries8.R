library(forecast)
library(astsa)
source('helper.R')
source('test.R')
require(randomForest)

data <- read.csv(file="GreenwichLondonAirQuality.csv", header=TRUE)

NO <- (data[data$Species == "NO",])
NO2 <- (data[data$Species == "NO2",])
NOX <- (data[data$Species == "NOX",])
PM10 <- (data[data$Species == "PM10",])
PM2.5 <- (data[data$Species == "PM2.5",])
WDIR <- (data[data$Species == "WDIR",])
WSPD <- (data[data$Species == "WSPD",])

data <- data.frame(
  "NO"=(get.FillValueForTimeSeries(NO))$Value, 
  "NO2"=(get.FillValueForTimeSeries(NO2))$Value, 
  "NOX"=(get.FillValueForTimeSeries(NOX))$Value, 
  "PM10"=(get.FillValueForTimeSeries(PM10))$Value, 
  "PM2.5"=(get.FillValueForTimeSeries(PM2.5))$Value,
  "WSPD"=(get.FillValueForTimeSeries(WSPD))$Value,
  "WDIR"=(get.FillValueForTimeSeries(WDIR))$Value
);
rm(NO, NO2, NOX, PM10, PM2.5, WDIR, WSPD)
data <- get.ClusterDataFrame(data)


# Plotting dataTs
N <- 168
plot(data[1:N,])
plot.ts(
  ts(data[1:N, "NO"], start=1, frequency=24),
  col=data[1:N, "NOCluster"],
)

# random forest
plot(data$NO[1:N], data$NOCluster[1:N], col=data$NOCluster[1:N])
forest <- randomForest(NOCluster ~ NO, data=data[1:N,], importance=T, do.trace=100)
forestPredicts <- predict(forest, newdata=data[1:N,])
points(data$NO[1:N], forestPredicts)


# forecasting by SARIMA
hoursToPredict <- 48
predictStart <- N+1
predictEnd <- predictStart+hoursToPredict-1

model <- sarima.for(
  ts(data$NO[1:N], start=1, frequency=24), 
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)

lines(ts(data$NO[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")


# classification by random forest
model$NO <- model$pred
classes <- predict(forest, newdata=model)
plot(data$NO[predictStart:predictEnd], data$NOCluster[predictStart:predictEnd], col="green")
points(data$NO[predictStart:predictEnd], classes, col="red")
