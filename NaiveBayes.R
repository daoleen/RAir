library('e1071')
require(ggplot2)
require(randomForest)
data <- read.csv(file="GreenwichLondonAirQuality.csv", header=TRUE)

data <- data[complete.cases(data$Value),]
data <- data[complete.cases(data$ReadingDateTime),]

# Map
NO <- data[data$Species == "NO", c(3,4)]
NO$Hours <- 0

for(i in 2 : length(NO$ReadingDateTime)) {
  date2 <- as.POSIXlt(NO$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  NO$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(NO$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

NO.clusters <- kmeans(NO$Value, centers=4)
NO$Cluster <- factor(NO.clusters$cluster)

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
