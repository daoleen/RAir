require(ggplot2)
require(randomForest)

data <- read.csv(file="GreenwichLondonAirQuality.csv", header=TRUE)

#clearing data
#dataFail <- (data[is.na(data$Value),])  # failed data
data <- data[complete.cases(data$Value),]
data <- data[complete.cases(data$ReadingDateTime),]
# add the vibras in future

# Map
NO <- data[data$Species == "NO",]
NO2 <- data[data$Species == "NO2",]
NOX <- data[data$Species == "NOX",]
PM10 <- data[data$Species == "PM10",]
PM2.5 <- data[data$Species == "PM2.5",]
WDIR <- data[data$Species == "WDIR",]
WSPD <- data[data$Species == "WSPD",]

NO$Hours <- NULL
NO$Hours[1] <- 0

for(i in 2 : length(NO$ReadingDateTime)) {
  date2 <- as.POSIXlt(NO$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  NO$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(NO$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

NO.clusters <- kmeans(NO$Value, centers=4)
NO$Cluster <- NO.clusters$cluster
NO.week <- (NO[NO$Hours < 168,])
NO.week <- NO.week[complete.cases(NO.week$ReadingDateTime),]

# Plot
ggplot(NO, aes(x = Hours, y = Value)) + geom_point(aes(color=(factor(Cluster)), group=Cluster))


# FOREST
N = 168;
rf <- randomForest(Value ~ Hours, data=NO[1:N,], importance=T, do.trace=100)
pred <- predict(rf)
plot(NO$Hours[1:N], NO$Value[1:N]) 
plot(NO$Hours[1:N], pred)
