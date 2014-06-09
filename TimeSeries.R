library('e1071')
require(ggplot2)
require(randomForest)
require(party)
source('helper.R')
data <- read.csv(file="GreenwichLondonAirQuality.csv", header=TRUE)

data <- data[complete.cases(data$Value),]
data <- data[complete.cases(data$ReadingDateTime),]

# Map
NO <- data[data$Species == "NO", c(3,4)]

# Week prediction
NO.week.values <- get.approxNaValues(NO[1:168,])
NO.week.kmeans <- kmeans(NO.week.values, centers=4)
NO.week.cluster <- factor(NO.week.kmeans$cluster)
NO.week.ts <- ts(NO.week.cluster, start=1, frequency=24)
plot(NO.week.ts)
NO.week.model <- ctree(NO.week.cluster ~ NO.week.ts, 
                       controls=ctree_control(minsplit=30, minbucket=10, maxdepth=5)
)
NO.week.preds <- predict(NO.week.model)
NO.week.preds.ts <- ts(NO.week.preds, start=1, frequency=24)
lines(NO.week.preds.ts, col="green")
# 100% model fit!!!

# Next 3 days test data
NO.week.next3.values <- get.approxNaValues(NO[169:240,])
NO.week.next3.kmeans <- kmeans(NO.week.next3.values, centers=4)
NO.week.next3.cluster <- factor(NO.week.next3.kmeans$cluster)
NO.week.next3.ts <- ts(NO.week.next3.cluster, start=8, frequency=24)
plot(NO.week.next3.ts)

# Next 3 days predict data
NO.week.next3.preds.ts <- ts(start=c(8,1), end=c(10,24), frequency=24)
NO.week.next3.preds <- predict(NO.week.model, newdata=NO.week.next3.preds.ts)
lines(NO.week.next3.preds, col="blue")


NO.week.values <- get.approxNaValues(NO[1:168,])
clusters <- kmeans(NO.week.values, centers=4)
NO.week.cluster <- factor(clusters$cluster)

NO.week.ts <- ts(NO.week.cluster, start=1, frequency=24)
#NO.week5.ts <- ts(start=1, end=5, frequency=24)
NO.week.last2.ts <- ts(start=5, end=7, frequency=24)

# For first 5-day
clusters <- kmeans(NO.week.values[1:120], centers=4)
NO.week5.cluster <- factor(clusters$cluster)
NO.week5.ts <- ts(NO.week5.cluster, start=1, frequency=24)
NO.week.model <- ctree(NO.week5.cluster ~ NO.week5.ts, controls=ctree_control(minsplit=30, minbucket=10, maxdepth=5))
plot(NO.week.model)

plot(NO.week.ts)

NO.week5.preds <- predict(NO.week.model)
NO.week5.preds.ts <- ts(NO.week5.preds, start=1, frequency=24)
lines(NO.week5.preds.ts, col="red")

# New data predictions
NOThreeDays.values <- get.approxNaValues(NO[1:72,])
clusters <- kmeans(NOTwoDays.values, centers=4)
NOThreeDays.cluster <- factor(clusters$cluster)
NOThreeDays.ts <- ts(NOThreeDays.cluster, start=1, frequency=24)
plot(NOThreeDays.ts)
NOThreeDays.ts <- ts(start=2, end=3, frequency=24)
NOThreeDays.preds <- predict(NOTwoDays.model, newdata=NOThreeDays.ts)
NOThreeDays.preds.ts <- ts(NOThreeDays.preds, start=2, frequency=24)
lines(NOThreeDays.preds.ts, col="red")




NO$Hours <- 0

get.approxNaValues(NO[1:168,])


for(i in 2 : length(NO$ReadingDateTime)) {
  date2 <- as.POSIXlt(NO$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  NO$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(NO$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

# Seasonal
no.ts <- ts(NO$Value[1:168], frequency=24)
no.test.ts <- ts(NO$Value[169:240], start=8, frequency=24)
plot(no.ts)
no.ts.dec <- decompose(no.ts)
plot(no.ts.dec)
fit <- arima(no.ts, order=c(0,0,0), list(order=c(2,1,0), prediod=24))
pred <- predict(fit, n.ahead=72)
U <- pred$pred + 2*pred$se
L <- pred$pred - 2*pred$se
ts.plot(no.ts, pred$pred, U, L, col=c(1,2,4,4), lty=c(1,1,2,2))
lines(no.test.ts, col="green")
no.clusters <- kmeans(no.ts, centers=4)
no.pred.clusters <- kmeans(pred$pred, centers=4)
no.test.clusters <- kmeans(no.test.ts, centers=4)
no.ts.clusters <- ts(no.clusters$cluster, frequency=24)
no.test.clusters <- ts(no.test.clusters$cluster, start=8, frequency=24)
no.pred.clusters <- ts(no.pred.clusters$cluster, start=8, frequency=24)
plot(no.ts.clusters)
lines(no.test.clusters, col="green")
lines(no.pred.clusters, col="red")

NO.clusters <- kmeans(NO$Value, centers=4)
NO$Cluster <- factor(NO.clusters$cluster)


# Classification
no.ts <- ts(NO$Cluster[1:96], frequency=24)
plot(no.ts)
library(party)
ct <- ctree(NO$Cluster[1:96] ~ no.ts, 
            controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
pClassId <- predict(ct)
table(NO$Cluster[1:96], pClassId)
# accuracy
(sum(NO$Cluster[1:96]==pClassId)) / 96
pred.ts <- ts(pClassId, frequency=24)
lines(pred.ts, col="red")
?predict
x <- ts(start=96, end=120, frequency=24)
predict(ct, newdata=x)

NO.week <- (NO[NO$Hours < 168, c(3,4)])
NO.week <- NO.week[complete.cases(NO.week$Cluster),]

# Plot
ggplot(NO, aes(x = Hours, y = Value)) + geom_point(aes(color=(factor(Cluster)), group=Cluster))

ggplot(NO.week, aes(x = Hours, y = Cluster)) + geom_point(aes(color=(factor(Cluster)), group=Cluster))
plot(y = NO.week$Cluster, x=NO.week$Hours, type="b")


# NaiveBayes
trainCount <- 100;
testCount <- 60;
train <- NO.week[1:trainCount,]
test <- NO.week[trainCount:(trainCount+testCount),]
#predicted <- data.frame("Hours"=NO.week[118,]$Hours, "Cluster"=NO.week[118,]$Cluster)
m <- naiveBayes(train, train$Cluster)
p <- predict(m, test, type="class")
lines(y=p, x=test$Hours, col="red")


# FOREST
N = 168;
f <- factor(c(rep("1", 10), rep("2", 10), rep("3", 20), rep("4", 29)), levels=c("1", "2", "3", "4"))
train <- NO.week[1:100,]
test <- NO.week[100:168,]
predicted <- data.frame("Hours"=test$Hours, "Cluster"=f)
m <- randomForest(train, train$Cluster)
p <- predict(m, predicted, type="response")

lines(y=p, x=test$Hours, col="red")
