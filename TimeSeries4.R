library(forecast)
source('helper.R')
source('test.R')

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
  "WSPD"=(get.FillValueForTimeSeries(WSPD))$Value[1:N]
);

pollutants.kmeans <- kmeans(pollutants, centers=4)
pollutants$cluster <- factor(pollutants.kmeans$cluster, levels=c('1', '2', '3', '4'))

plot(pollutants)
pollutants.ts <- ts(pollutants$cluster, start=1, frequency=24)
plot(pollutants.ts)

# fit <- auto.arima(pollutants.ts)
fit <- ets(pollutants.ts)
preds <- forecast(fit, 3)
plot(preds)
