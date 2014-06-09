get.posixlt <- function(date) {
  return (as.POSIXlt(date, tz="", format=c('%d/%m/%Y %H:%M')));
}

get.difftimeInHours <- function(timeStart, timeEnd) {
  return (as.numeric(difftime(timeEnd, timeStart, units="hours")));
}


get.approxNaValues <- function(dataset) {  
  dateprev <- get.posixlt(dataset$ReadingDateTime[1]);
  values <- c(dataset$Value[1]);
  
  for(i in 2 : length(dataset$ReadingDateTime)) {
    date <- get.posixlt(dataset$ReadingDateTime[i]);
    
    if(is.na(date)) {
      # add 1 hour to date
      date <- dateprev + 60
    }
    
    if(abs(get.difftimeInHours(dateprev, date)) != 1) {
      emptyDataCount <- abs(get.difftimeInHours(dateprev, date))-1;
      prevIndex <- i-emptyDataCount-1;
      mean <- abs(( (dataset$Value[i]) - (dataset$Value[i-1]) ) / (emptyDataCount+1));
      for(e in 1 : emptyDataCount) {
        value <- values[length(values)] + mean
        if(is.nan(value) || is.infinite(value) || is.na(value)) {
          value <- values[length(values)-1]
          print(value)
        }
        values <- c(values, value);
      }
    }
    
    values <- c(values, dataset$Value[i]);
    dateprev <- date;
  }
  
  return (values);
}


get.FillValueForTimeSeries <- function(data) {
  length <- length(data$ReadingDateTime)
  
  for(i in 1 : length) {
    if(is.na(data$Value[i])) {
      j <- i;
      sub <- TRUE;
      
      while(is.na(data$Value[j])) {
        if(j-24 > 0 && sub) {
          j = j - 24;
        }
        else if(j + 24 <= length) {
          sub <- FALSE;
          j = j + 24;
        }
        else {
          print('Can\'t fill the empty data because out of bounds')
          break
        }
      }
      
      data$Value[i] <- data$Value[j];
    }
  }
  
  return(data)
}


get.ColorForPlot <- function(clusters) {
  return (
    lapply(clusters, function(cluster){
      switch(as.character(cluster), 
             "Low"="green",
             "Medium"="red",
             "High"="blue",
             "Very High"="black"
      )
    })
  )
}

get.ClusterDataFrame <- function(dataFrame) {
  lvls <- c("1", "2", "3", "4")
#  lbls <- c("Low", "Medium","High", "Very High")
lbls <- "";
  
  dataFrame$NOCluster <- factor(
    kmeans(dataFrame$NO, centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  dataFrame$NO2Cluster <- factor(
    kmeans(dataFrame$NO2, centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  dataFrame$NOXCluster <- factor(
    kmeans(dataFrame$NOX, centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  dataFrame$PM10Cluster <- factor(
    kmeans(dataFrame$PM10, centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  dataFrame$PM25Cluster <- factor(
    kmeans(dataFrame$PM2.5, centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  dataFrame$WDIRCluster <- factor(
    kmeans(dataFrame$WDIR,  centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  dataFrame$WSPDCluster <- factor(
    kmeans(dataFrame$WSPD, centers=4)$cluster, 
    levels=lvls,
    labels=lbls,
    ordered=TRUE
  );
  
  return (dataFrame)
}


airquality.forecast <- function(data, start=1, trainCount=168, hoursToPredict=72) {
  N <- start+trainCount-1
  
  plot(data[start:N,])
  plot.ts(ts(data[start:N, "NO"], start=1, frequency=24), col=data[start:N, "cluster"], ylab="NO")
  plot.ts(ts(data[start:N, "NO2"], start=1, frequency=24),  col=data[start:N, "cluster"], ylab="NO2")
  plot.ts(ts(data[start:N, "NOX"], start=1, frequency=24), col=data[start:N, "cluster"], ylab="NOX")
  plot.ts(ts(data[start:N, "PM10"], start=1, frequency=24),col=data[start:N, "cluster"], ylab="PM10")
  plot.ts(ts(data[start:N, "PM2.5"], start=1, frequency=24), col=data[start:N, "cluster"], ylab="PM2.5")
  plot.ts(ts(data[start:N, "WSPD"], start=1, frequency=24), col=data[start:N, "cluster"], ylab="WSPD")
  plot.ts(ts(data[start:N, "WDIR"], start=1, frequency=24), col=data[start:N, "cluster"], ylab="WDIR")
  
  # Decomposing for seasons and trends for range of data
  oldPar <- par(no.readonly=TRUE)
  plot(decompose(ts(data$NO[start:N], start=1, frequency=24)))
  plot(decompose(ts(data$NO2[start:N], start=1, frequency=24)))
  plot(decompose(ts(data$NOX[start:N], start=1, frequency=24)))
  plot(decompose(ts(data$PM10[start:N], start=1, frequency=24)))
  plot(decompose(ts(data$PM2.5[start:N], start=1, frequency=24)))
  plot(decompose(ts(data$WSPD[start:N], start=1, frequency=24)))
  plot(decompose(ts(data$WDIR[start:N], start=1, frequency=24)))
  par(oldPar)
  
  # forecasting by SARIMA
  predictStart <- N+1
  predictEnd <- predictStart+hoursToPredict-1
  
  warning(sprintf("Train data range: [%d;%d]", start, N))
  warning(sprintf("Predict data range: [%d;%d]", predictStart, predictEnd))
  
  modelNO <- sarima.for(
    ts(data$NO[start:N], start=(start-1)/24+1, frequency=24), 
    hoursToPredict, 2, 1, 1, 0, 1, 3, 24
  )
  lines(ts(data$NO[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")
  
  modelNO2 <- sarima.for(
    ts(data$NO2[start:N], start=(start-1)/24+1, frequency=24),
    hoursToPredict, 2, 1, 1, 0, 1, 3, 24
  )
  lines(ts(data$NO2[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")
  
  modelNOX <- sarima.for(
    ts(data$NOX[start:N], start=(start-1)/24+1, frequency=24),
    hoursToPredict, 2, 1, 1, 0, 1, 3, 24
  )
  lines(ts(data$NOX[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")
  
  modelPM10 <- auto.arima(ts(data$PM10[start:N], start=(start-1)/24+1, frequency=24))
  modelPM10$pred <- forecast(modelPM10, h=hoursToPredict)$mean
  plot(ts(modelPM10$pred, start=N/24+1, frequency=24), type="o", col="red")
  lines(ts(data$PM10[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="green")
  
  modelPM2.5 <- sarima.for(
    ts(data$PM2.5[start:N], start=(start-1)/24+1, frequency=24),
    hoursToPredict, 2, 1, 1, 0, 1, 3, 24
  )
  lines(ts(data$PM2.5[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")
  
  modelWSPD <- sarima.for(
    ts(data$WSPD[start:N], start=(start-1)/24+1, frequency=24),
    hoursToPredict, 2, 1, 1, 0, 1, 3, 24
  )
  lines(ts(data$WSPD[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")
  
  modelWDIR <- sarima.for(
    ts(data$WDIR[start:N], start=(start-1)/24+1, frequency=24),
    hoursToPredict, 2, 1, 1, 0, 1, 3, 24
  )
  lines(ts(data$WDIR[predictStart:predictEnd], start=N/24+1, frequency=24), type="o", col="blue")
  
  
  # classification
  model <- data.frame(
    "NO" = modelNO$pred,
    "NO2" = modelNO2$pred,
    "NOX" = modelNOX$pred,
    "PM10" = modelPM10$pred,
    "PM2.5" = modelPM2.5$pred,
    "WSPD" = modelWSPD$pred,
    "WDIR" = modelWDIR$pred
  );
  
  net <- nnet(cluster ~ NO+NO2+NOX+PM10+PM2.5+WSPD+WDIR, data=data[start:N,], size=8)
  classes <- predict(net, newdata=model, type="class")
  warning("Count of predicted classes is: %d", length(classes))
  
  plot(ts(data$cluster[predictStart:predictEnd], start=N/24+1, frequency=24), col="green", type="o")
  points(ts(classes, start=N/24+1, frequency=24), col="red")
  
  # computing accuracy
  accuracy <- length(classes[data$cluster[predictStart:predictEnd] == classes]) / length(classes)
  warning(sprintf("Accuracy is: %s%%", round(accuracy*100, 2)))
  
  return(accuracy)
}

airquality.forecast.byweek <- function(data, week) {
  if(week * 168 + 72 <= length(data$NO)) {
    res <- try(airquality.forecast(data=data, start=(week-1)*168+1))
    if(class(res) == "try-error") {
      return (0)
    }
    return (res)
  }
  warning("There are is no enough data for compute!")
}

movingAverage <- function(data) {
  m <- length(data[,1])
  datanames <- names(data)
  first <- data[1,]
  last <- data[m,]
  data <- as.data.frame(filter(data, sides=2, rep(1/3,3)))
  names(data) <- datanames
  data[1,] <- first
  data[m,] <- last
  return(data)
}