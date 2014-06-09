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
  "WSPD"=(get.FillValueForTimeSeries(WSPD))$Value[1:N]
);

pollutants.NO.ts <- ts(pollutants$NO, start=1, frequency=24)
summary(pollutants.NO.ts)
plot.ts(pollutants.NO.ts)

summary(fit <- lm(pollutants.NO.ts ~ time(pollutants.NO.ts)))
plot(pollutants.NO.ts, type="o", ylab="NO")
abline(fit)


# Detrending the series
fit <- lm(pollutants.NO.ts ~ time(pollutants.NO.ts))
par(mfrow=c(3,1))
plot(pollutants.NO.ts, type="o", ylab="NO", main="original")
plot(resid(fit), type="o", main="detrended")
plot(diff(pollutants.NO.ts), type="o", main="first difference")

# plot ACFs
par(mfrow=c(3,1))
