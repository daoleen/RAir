library(forecast)
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



# pollutants <- data.frame("NO"=get.approxNaValues(NO), "NO2"=get.approxNaValues(NO2), "NOX"=get.approxNaValues(NOX), 
#                        "PM10"=get.approxNaValues(PM10), "PM2.5"=get.approxNaValues(PM2.5), 
#                        "WDIR"=get.approxNaValues(WDIR), "WSPD"=get.approxNaValues(WSPD));

N <- 168;
pollutants <- data.frame(
  "NO"=(get.FillValueForTimeSeries(NO))$Value[1:N], 
  "NO2"=(get.FillValueForTimeSeries(NO2))$Value[1:N], 
  "NOX"=(get.FillValueForTimeSeries(NOX))$Value[1:N], 
  "PM10"=(get.FillValueForTimeSeries(PM10))$Value[1:N], 
  "PM2.5"=(get.FillValueForTimeSeries(PM2.5))$Value[1:N],
  "WSPD"=(get.FillValueForTimeSeries(WSPD))$Value[1:N],
  "WDIR"=(get.FillValueForTimeSeries(WDIR))$Value[1:N]
);

pollutants.cluster <- data.frame(
  "NO" = factor(
    kmeans(get.FillValueForTimeSeries(NO), centers=4)$cluster, 
    levels=c("1", "2", "3", "4"),
    labels=c("Low", "Medium","High", "Very High"),
    ordered=TRUE
  ),
  "NOX" = factor(
    kmeans(get.FillValueForTimeSeries(NOX), centers=4)$cluster, 
    levels=c("1", "2", "3", "4"),
    labels=c("Low", "Medium","High", "Very High"),
    ordered=TRUE
  )
);

pollutants.ts <- ts(pollutants, start=1, frequency=24)
plot.ts(pollutants.ts[,"NO"], type="o", col=as.numeric(pollutants.cluster$NO))



# random forest
plot(pollutants$NO, pollutants.cluster$NO, col=as.numeric(pollutants.cluster$NO))
forest <- randomForest(pollutants.cluster$NO ~ pollutants$NO, importance=T, do.trace=100)
forestPredicts <- predict(forest, newdata=pollutants$NO)
points(pollutants$NO, forestPredicts)


#legend(locator(1), title="Classess", levels(pollutants$cluster), pch=c(16), cex=0.4, horiz=T,
#       col=c("Low"="1", "Medium"="2", "High"="3", "Very High"="4")
#);

hoursToPredict <- 48
predictStart <- N+1
predictEnd <- predictStart+hoursToPredict-1
real.no <- get.FillValueForTimeSeries(NOX)$Value[predictStart:predictEnd]
real.no.ts <- ts(real.no, start=N/24+1, frequency=24)

model <- sarima.for(ts(pollutants.ts[,"NOX"], start=1, frequency=24), hoursToPredict, 2, 1, 1, 0, 1, 3, 24)
lines(real.no.ts, type="o", col="blue")


# Classification for new data by random forest
classes <- predict(forest, x=as.numeric(model$pred))

# Plotting for real data
pollutants.cluster$NO[predictStart:predictEnd]
