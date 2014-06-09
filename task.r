require(ggplot2)
library(e1071)

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

NO2$Hours <- NULL
NO2$Hours[1] <- 0

for(i in 2 : length(NO2$ReadingDateTime)) {
  date2 <- as.POSIXlt(NO2$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  NO2$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(NO2$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

NOX$Hours <- NULL
NOX$Hours[1] <- 0

for(i in 2 : length(NOX$ReadingDateTime)) {
  date2 <- as.POSIXlt(NOX$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  NOX$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(NOX$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

PM10$Hours <- NULL
PM10$Hours[1] <- 0

for(i in 2 : length(PM10$ReadingDateTime)) {
  date2 <- as.POSIXlt(PM10$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  PM10$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(PM10$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

PM2.5$Hours <- NULL
PM2.5$Hours[1] <- 0

for(i in 2 : length(PM2.5$ReadingDateTime)) {
  date2 <- as.POSIXlt(PM2.5$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  PM2.5$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(PM2.5$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

WDIR$Hours <- NULL
WDIR$Hours[1] <- 0

for(i in 2 : length(WDIR$ReadingDateTime)) {
  date2 <- as.POSIXlt(WDIR$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  WDIR$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(WDIR$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

WSPD$Hours <- NULL
WSPD$Hours[1] <- 0

for(i in 2 : length(WSPD$ReadingDateTime)) {
  date2 <- as.POSIXlt(WSPD$ReadingDateTime[i], tz="", format=c('%d/%m/%Y %H:%M'))
  WSPD$Hours[i] <- as.numeric(difftime(date2, as.POSIXlt(WSPD$ReadingDateTime[1], tz="", format=c('%d/%m/%Y %H:%M')), units="hours"))
}

NO.clusters <- kmeans(NO$Value, centers=4)
NO2.clusters <- kmeans(NO2$Value, centers=4)
NOX.clusters <- kmeans(NOX$Value, centers=4)
PM10.clusters <- kmeans(PM10$Value, centers=4)
PM2.5.clusters <- kmeans(PM2.5$Value, centers=4)
WDIR.clusters <- kmeans(WDIR$Value, centers=4)
WSPD.clusters <- kmeans(WSPD$Value, centers=4)
NO$Cluster <- NO.clusters$cluster
NO2$Cluster <- NO2.clusters$cluster
NOX$Cluster <- NOX.clusters$cluster
PM10$Cluster <- PM10.clusters$cluster
PM2.5$Cluster <- PM2.5.clusters$cluster
WDIR$Cluster <- WDIR.clusters$cluster
WSPD$Cluster <- WSPD.clusters$cluster


NO.week <- (NO[NO$Hours < 168,])
NO.week <- NO.week[complete.cases(NO.week$ReadingDateTime),]


# Plot
#ggplot(NO[1:100, 1:length(NO)], aes(x = Hours, y = Value)) + geom_point(aes(color=(factor(Cluster)), group=Cluster))
#ggplot(NO.week, aes(x = Hours, y = Value)) + geom_point(aes(color=(factor(Cluster)), group=Cluster))
ggplot(NO, aes(x = Hours, y = Value)) + geom_point(aes(color=(factor(Cluster)), group=Cluster))


NO.week.next3days <- (NO[NO$Hours >= 168 & NO$Hours < 168+3*24,])
#NO.week.next3days$Hours <- NULL;
NO.week.next3days$Value <- 0;
NO.week.next3days$Cluster <- NULL;
NO.week.next3days$Site <- NULL;
NO.week.next3days$Species <- NULL;
NO.week.next3days$ReadingDateTime <- NULL;
NO.week.next3days$Units <- NULL;
NO.week.next3days$Provisional.or.Ratified <- NULL;
NO.week.next3days <- NO.week.next3days[complete.cases(NO.week.next3days$Value),]

model <- svm(Cluster~Value, data=NO.week, kernel="polynomial")
preds <- predict(model, NO.week.next3days)



# GLM
preds <- glm(Cluster~Value, data=NO.week)
preds <- predict(preds,expand.grid(NO.week.next3days))



# FOREST (working TRUE!!!)
N = 5000;
rf <- randomForest(Value ~ Hours, data=NO[1:N,], importance=T, do.trace=100)
pred <- predict(rf)
plot(NO$Hours[1:N], NO$Value[1:N]) 
plot(NO$Hours[1:N], pred)


?randomForest



lm.k <- glm(Cluster ~ (-1)^Hours +  Hours, data=NO.week)
preds <- predict(lm.k, newdata=expand.grid(NO.week.next3days))

# Your chanes will be below