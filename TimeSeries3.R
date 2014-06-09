library(forecast)
source('helper.R')
data <- read.csv(file="GreenwichLondonAirQuality.csv", header=TRUE)

data <- data[complete.cases(data$Value),]
data <- data[complete.cases(data$ReadingDateTime),]

# Map
NO <- data[data$Species == "NO", c(3,4)]
NO.values <- get.approxNaValues(NO)
NO.clusters <- (kmeans(NO.values, centers=4))$cluster
# NO$Cluster <- NO.clusters$cluster

NO.data.values <- NO.values[1:43776]  # 1824 days
NO.data.clusters.ts <- ts(NO.clusters[1:43776], start=1, frequency=24)
NO.data.ts <- ts(NO.data.values, start=1, frequency=24)
plot(NO.data.ts)

NO.test.values <- NO.values[43777:43836]
NO.test.clusters.ts <- ts(NO.clusters[43777:43836], start=1825, frequency=24)
NO.test.ts <- ts(NO.test.values, start=1825, frequency=24)

# fit <- stl(NO.week.ts, s.window="periodic")
# fit <- arima(NO.week.ts)
fit <- auto.arima(NO.data.ts)
# fit <- ets(NO.year.ts)
# fit <- HoltWinters(NO.year.ts)

# accuracy of fitting the model
accuracy(fit)

# predict next 3 days
preds <- forecast(fit, 60)
plot(preds)
lines(ts(NO.test.values, start=366, frequency=24), col="green")

# scales
preds.ts <- ts(preds$mean, start=c(1825,1), frequency=24)
plot(ts(NO.test.values, start=c(366,1), frequency=24), col="black")
plot(preds.ts, col="blue")


# Clustering all data
preds.data <- c(NO.values[1:8760], preds$mean)
preds.data.clusters <- (kmeans(preds.data, centers=4))$cluster
preds.clusters <- preds.data.clusters[8761:8832]
preds.clusters.ts <- ts(preds.clusters, start=c(366,1), end=c(368,24), frequency=24)
real.clusters.ts <- ts(NO.clusters[8761:8832],start=c(366,1), end=c(368,24), frequency=24)
plot(real.clusters.ts)
lines(preds.clusters.ts, col="blue")

# plotting clusters
plot(ts(start=c(1,1), end=c(368,24), frequency=24), ylim=c(1, 4), type="p")
lines(NO.year.clusters.ts, type="p")
lines(preds.clusters.ts, col="red", type="b")

# real data
NO.next3.ts <- ts(NO.clusters[8761:8832], start=366, frequency=24)
lines(NO.next3.ts, col="green", type="b")


# scale plot clusters
plot(ts(start=c(366,1), end=c(368,24), frequency=24), ylim=c(1, 4), type="p")
lines(NO.next3.ts, col="black", type="p")
lines(preds.clusters.ts, col="blue", type="p")

# accuracy
(sum(NO.clusters[8761:8832]==preds.clusters)) / length(preds.clusters)
(sum(ts(NO.values[8761:8832], start=366, frequency=24)==preds$mean)) / length(preds$mean)
