library(forecast)
library(astsa)
source('helper.R')
source('test.R')
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
data <- as.data.frame(scale(data))

# Clustering
cluster <- factor(
  kmeans(data, centers=4)$cluster,
  levels=c("1","2","3","4"),
  ordered=TRUE
)

#data <- movingAverage(data)
data$cluster <- cluster
rm(NO, NO2, NOX, PM10, PM2.5, WDIR, WSPD, cluster)


# Decomposing for seasons and trends for all period
oldPar <- par(no.readonly=TRUE)
plot(decompose(ts(data$NO, start=1, frequency=24)))
plot(decompose(ts(data$NO2, start=1, frequency=24)))
plot(decompose(ts(data$NOX, start=1, frequency=24)))
plot(decompose(ts(data$PM10, start=1, frequency=24)))
plot(decompose(ts(data$PM2.5, start=1, frequency=24)))
plot(decompose(ts(data$WSPD, start=1, frequency=24)))
plot(decompose(ts(data$WDIR, start=1, frequency=24)))
par(oldPar)

# accuracy <- c()
weeksCount <- floor(length(data$NO)/168)
start <- 68

accuracy <- airquality.forecast.byweek(data, 7)

for(i in start : weeksCount) {
  #accuracy <- c(accuracy, airquality.forecast.byweek(data, i))
}

save('accuracy', file='accuracy.dat')
