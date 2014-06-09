library(forecast)
library(randomForest)
source('helper.R')
data <- read.csv(file="GreenwichLondonAirQuality.csv", header=TRUE)

data <- data[complete.cases(data$Value),]
data <- data[complete.cases(data$ReadingDateTime),]

# Map
NO <- data[data$Species == "NO", c(3,4)]
NO.values <- get.approxNaValues(NO)
NO.clusters <- (kmeans(NO.values, centers=4))$cluster
# NO$Cluster <- NO.clusters$cluster


# Classification with automated exponential forecasting
NO.week.values <- get.approxNaValues(NO[1:168,])
NO.week.clusters.ts <- ts(NO$Cluster[1:168], start=1, frequency=24)
NO.week.ts <- ts(NO.week.values, start=1,frequency=24)
plot(NO.week.ts)

# fit <- stl(NO.week.ts, s.window="periodic")
# fit <- arima(NO.week.ts)
# fit <- auto.arima(NO.week.ts)
fit <- ets(NO.week.ts)
# fit <- HoltWinters(NO.week.ts)

# accuracy of fitting the model
accuracy(fit)

# predict next 3 days
preds <- forecast(fit, 72)
plot(preds)

# prediction clusters
preds.clusters <- kmeans(preds$mean, centers=4)
preds.clusters.ts <- ts(preds.clusters$cluster, start=c(8,1), end=c(10,24), frequency=24)

# plotting clusters
plot(ts(start=c(1,1), end=c(10,24), frequency=24), ylim=c(1, 4), type="p")
lines(NO.week.clusters.ts, type="p")
lines(preds.clusters.ts, col="red", type="p")

# real data
NO.next3.ts <- ts(NO$Cluster[169:240], start=8, frequency=24)
lines(NO.next3.ts, col="green", type="p")






# Classification with automated exponential forecasting
# FOR THE 365 days!!!
NO.year.values <- NO.values[1:8760]
NO.year.clusters.ts <- ts(NO.clusters[1:8760], start=1, frequency=24)
NO.year.ts <- ts(NO.year.values, start=1,frequency=24)
plot(NO.year.ts)

# fit <- stl(NO.week.ts, s.window="periodic")
# fit <- arima(NO.week.ts)
 fit <- auto.arima(NO.year.ts)
# fit <- ets(NO.year.ts)
# fit <- HoltWinters(NO.year.ts)

# accuracy of fitting the model
accuracy(fit)

# predict next 3 days
preds <- forecast(fit, 72)
plot(preds)
lines(ts(NO.values[8761:8832], start=366, frequency=24), col="green")

# scales
preds.ts <- ts(preds$mean, start=c(366,1), end=c(368,24), frequency=24)
plot(ts(NO.values[8761:8832], start=366, frequency=24), col="black")
lines(preds.ts, col="blue")

# prediction clusters
# !!!!!! Здесь использовать не kmeans!!!!
# ТК kmeans разбивает на новые группы
# А НУЖНО РАЗБИВАТЬ НА ОСНОВАНИИ ГРУПП ВСЕГО ДАТАСЕТА
# ПОЭТОМУ ИСПОЛЬЗОВАТЬ КЛАССИФИКАЦИЮ!!!
preds.clusters <- kmeans(preds$mean, centers=4)
preds.clusters.ts <- ts(preds.clusters$cluster, start=c(366,1), end=c(368,24), frequency=24)

# ctree classification
N <- 8760;
#forestModel <- randomForest(factor(NO.clusters[1:8760]) ~ NO.values[1:8760], importance=T, do.trace=100)
ctreeModel <- ctree(factor(NO.clusters[1:8760]) ~ NO.values[1:8760], 
                       controls=ctree_control(minsplit=30, minbucket=10, maxdepth=5)
)
# НЕ РАБОТАЕТ predict!!! на выходе должно быть 72 значения,
# а он выкидывает 8760, т.е. старые предсказания
preds.clusters <- predict(ctreeModel, newx=preds$mean, n.ahead=72) # preds$mean  o_0
preds.clusters.ts <- ts(preds.clusters, start=c(366,1), end=c(368,24), frequency=24)


# kNN classification
require(class)
model <- knn(NO.values[1:8760], class=factor(NO.clusters[1:8760]), k=4)
preds.clusters <- predict(model, newdata=preds$mean)


# SVM
library(e1071)
model  <- svm(factor(NO.clusters[1:8760]) ~ NO.values[1:8760], kernel="radial", gamma=0.001, cost=10) 
summary(model)
preds.clusters <- predict(model, preds$mean)


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
