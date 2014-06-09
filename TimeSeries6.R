library(forecast)
source('helper.R')
source('test.R')
require(astsa)

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

pollutants$cluster <- factor(
  kmeans(pollutants, centers=4)$cluster, 
  levels=c("1", "2", "3", "4"),
  labels=c("Low", "Medium","High", "Very High"),
  ordered=TRUE
);

#pollutants.NO.ts <- ts(pollutants$NO, start=1, frequency=24)
pollutants.ts <- ts(pollutants, start=1, frequency=24)
attributes(pollutants.ts[,"NO"])
plot.ts(pollutants.ts, type="o", col=pollutants$cluster)


#legend(locator(1), title="Classess", levels(pollutants$cluster), pch=c(16), cex=0.4, horiz=T,
#       col=c("Low"="1", "Medium"="2", "High"="3", "Very High"="4")
#);

hoursToPredict <- 48
predictStart <- N+1
predictEnd <- predictStart+hoursToPredict-1
real.no <- get.FillValueForTimeSeries(NO)$Value[predictStart:predictEnd]
real.no.ts <- ts(real.no, start=N/24+1, frequency=24)

model <- sarima.for(ts(pollutants.ts[,"NO"], start=1, frequency=24), hoursToPredict, 2, 1, 1, 0, 1, 3, 24)
lines(real.no.ts, type="o", col="blue")
