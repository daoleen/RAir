library(forecast)
library(astsa)
source('helper.R')
source('test.R')
require(randomForest)
require(e1071)
require(nnet)
require(monmlp)
require(ROCR)

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
data <- as.data.frame(scale(data))
rm(NO, NO2, NOX, PM10, PM2.5, WDIR, WSPD)

# Clustering
data$cluster <- factor(
  kmeans(data, centers=4)$cluster,
  levels=c("1","2","3","4"),
  ordered=TRUE
)


# Plotting data
N <- 168
plot(data[1:N,])
plot.ts(ts(data[1:N, "NO"], start=1, frequency=6), col=data[1:N, "cluster"], ylab="NO")
plot.ts(ts(data[1:N, "NO2"], start=1, frequency=6),  col=data[1:N, "cluster"], ylab="NO2")
plot.ts(ts(data[1:N, "NOX"], start=1, frequency=6), col=data[1:N, "cluster"], ylab="NOX")
plot.ts(ts(data[1:N, "PM10"], start=1, frequency=6),col=data[1:N, "cluster"], ylab="PM10")
plot.ts(ts(data[1:N, "PM2.5"], start=1, frequency=6), col=data[1:N, "cluster"], ylab="PM2.5")
plot.ts(ts(data[1:N, "WSPD"], start=1, frequency=6), col=data[1:N, "cluster"], ylab="WSPD")
plot.ts(ts(data[1:N, "WDIR"], start=1, frequency=6), col=data[1:N, "cluster"], ylab="WDIR")

# Decomposing for seasons and trends for all period
plot(decompose(ts(data$NO, start=1, frequency=24)))
plot(decompose(ts(data$NO2, start=1, frequency=24)))
plot(decompose(ts(data$NOX, start=1, frequency=24)))
plot(decompose(ts(data$PM10, start=1, frequency=24)))
plot(decompose(ts(data$PM2.5, start=1, frequency=24)))
plot(decompose(ts(data$WSPD, start=1, frequency=24)))
plot(decompose(ts(data$WDIR, start=1, frequency=24)))

# Decomposing for seasons and trends for first week data
plot(decompose(ts(data$NO[1:168], start=1, frequency=24)))
plot(decompose(ts(data$NO2[1:168], start=1, frequency=24)))
plot(decompose(ts(data$NOX[1:168], start=1, frequency=24)))
plot(decompose(ts(data$PM10[1:168], start=1, frequency=24)))
plot(decompose(ts(data$PM2.5[1:168], start=1, frequency=24)))
plot(decompose(ts(data$WSPD[1:168], start=1, frequency=24)))
plot(decompose(ts(data$WDIR[1:168], start=1, frequency=24)))

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

modelPM10 <- auto.arima(ts(data$PM10[1:N], start=1, frequency=24))
modelPM10$pred <- forecast(modelPM10, h=hoursToPredict)$mean
plot(ts(modelPM10$pred, start=N/24+1, frequency=24), type="o", col="red")
lines(ts(data$PM10[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="green")

modelPM2.5 <- sarima.for(
  ts(data$PM2.5[1:N], start=1, frequency=24),
  hoursToPredict, 2, 1, 1, 0, 1, 3, 24
)
lines(ts(data$PM2.5[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")

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


# classification
model <- data.frame(
  "NO" = modelNO$pred,
  "NO2" = modelNO2$pred,
  "NOX" = modelNOX$pred,
  "PM10" = modelPM10$pred,
  "PM2.5" = modelPM2.5$pred,
  "WSPD" = modelWSPD$pred,
  "WDIR" = modelWDIR$pred
);

 net <- nnet(cluster ~ NO+NO2+NOX+PM10+PM2.5+WSPD+WDIR, data=data[1:N,], size=8)
 classes <- predict(net, newdata=model, type="class")
# computing accuracy
 accuracy <- length(classes[data$cluster[predictStart:predictEnd] == classes]) / length(classes)

#svm <- svm(cluster ~ NO+NO2+NOX+PM10+PM2.5+WSPD+WDIR, data=data[1:N,], type="nu-classification", 
#           kernel="sigmoid")
#classes <- predict(svm, newdata=model, type="class")
# computing accuracy
#accuracy <- length(classes[data$cluster[predictStart:predictEnd] == as.numeric(classes)]) / length(as.numeric(classes))     # accuracy for svm


#net <- monmlp.fit(x=as.matrix(data[1:N,1:7]), y=as.matrix(as.numeric(data[1:N, 8])), 
#                  hidden1=8, hidden2=8, n.trials=10, n.ensemble=3, bag=T)
#classes <- monmlp.predict(x=as.matrix(model), weights=net)
# computing accuracy
#accuracy <- length(classes[data$cluster[predictStart:predictEnd] == as.numeric(round(classes))]) 
#  / length(as.numeric(round(classes)))     # accuracy for monmlp

plot(ts(data$cluster[predictStart:predictEnd], start=N/24+1, frequency=24), col="green", type="o")
points(ts(classes, start=N/24+1, frequency=24), col="red")


sprintf("Accuracy is: %s%%", round(accuracy*100, 2))
