library(forecast)
library(astsa)
source('helper.R')
source('test.R')
require(randomForest)
require(nnet)

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
#data <- get.ClusterDataFrame(data)
data$cluster <- factor(
  kmeans(data, centers=4)$cluster,
  levels=c("1","2","3","4"),
  ordered=TRUE
)


# Plotting data
N <- 168
plot(data[1:N,])
plot.ts(
  ts(data[1:N, "NO"], start=1, frequency=24),
  col=data[1:N, "cluster"],
)

# random forest
#plot(data[1:N,c("NO", "NO2", "NOX", "PM10", "PM2.5", "WSPD", "WDIR")], data$cluster[1:N], col="green")
#forest <- randomForest(cluster ~ NO+NO2+NOX+WSPD+WDIR, data=data[1:N,], importance=T, do.trace=100, ntree=900)
#forestPredicts <- predict(forest, newdata=data[1:N,])
#lines(data[1:N,c("NO", "NO2", "NOX", "PM10", "PM2.5", "WSPD", "WDIR")], forestPredicts, col="red")
#net <- nnet(cluster ~ NO+NO2+NOX+WSPD+WDIR, data=data[1:N,], size=4)
net <- nnet(cluster ~ NO+NO2+NOX+WDIR, data=data[1:N,], size=4)


# forecasting by SARIMA
hoursToPredict <- 48
predictStart <- N+1
predictEnd <- predictStart+hoursToPredict-1

modelNO <- sarima.for(
  ts(data$NO[1:N], start=1, frequency=24), 
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)
lines(ts(data$NO[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")

modelNO2 <- sarima.for(
  ts(data$NO2[1:N], start=1, frequency=24),
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)
lines(ts(data$NO2[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")

modelNOX <- sarima.for(
  ts(data$NOX[1:N], start=1, frequency=24),
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)
lines(ts(data$NOX[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")

modelWSPD <- sarima.for(
  ts(data$WSPD[1:N], start=1, frequency=24),
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)
lines(ts(data$WSPD[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")

modelWDIR <- sarima.for(
  ts(data$WDIR[1:N], start=1, frequency=24),
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)
lines(ts(data$WDIR[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")


# classification by random forest
model <- data.frame(
  "NO" <- modelNO$pred,
  "NO2" <- modelNO2$pred,
  "NOX" <- modelNOX$pred,
#  "WSPD" <- modelWSPD$pred,
  "WDIR" <- modelWDIR$pred
);

classes <- predict(net, newdata=model, type="class")
plot(data$NO[predictStart:predictEnd], data$cluster[predictStart:predictEnd], col="green")
points(data$NO[predictStart:predictEnd], classes, col="red")
