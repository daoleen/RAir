# ---------------------------------------------------------------------------------------------------------
# Learning Example
# Input data set is provided. Reflets some simulated data set.
# Goal is to establish the decison boundary for the 2D data set via different methods
# The skeletons for Linerar Regression, ANN, and SVM are provided
# The students are supposed to:
# 1) 'optimize' the 3 provided (LR, ANN, SVM) skeletons
# 2) code 2 other methods that solve the problem
#
# A good read is "The Elements of Statistical Learning" by Hastie
# ---------------------------------------------------------------------------------------------------------



# ----------------------------------------------------------------
# Linear Regression Example

data <- read.csv('rawdata.txt', header=FALSE)
plot(data)
names(data) <- c("X1", "X2", "Y")
 
npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)

# Fitting the model
?glm()
p <- glm(Y~., data=data)
df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(p)))

?predict()
preds <- predict(p,expand.grid(df))
zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)

# Retrieve the dimension of an object
dim(zz)


x0 <- data[data$Y == 0, c(1,2)]
dim(x0)
x1 <- data[data$Y == 1, c(1,2)]
dim(x1)
symbols(x0, circles=rep(0.4, nrow(x0)), inches=FALSE, bg="cyan")
symbols(x1, squares=rep(0.6, nrow(x1)), inches=FALSE, add=TRUE, bg="gold")
?contour()
contour(x1vals, x2vals, zz, col="red", add=T, levels=(0.42))

# Assignment1: Can you find a better solution via the LR approach? Adjust some of the variables above


# ----------------------------------------------------------------
# ANN Example

?nnet()

library(nnet)
data <- read.csv('rawdata.txt', header=FALSE)
names(data) <- c("X1", "X2", "Y")

npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)

ep <- 0.001
p <- nnet(Y~., data=data, size=3, rang=ep, decay=0.0001, maxit=400)
df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(p)))

preds <- predict(p,expand.grid(df))
zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)
dim(zz)

x0 <- data[data$Y == 0, c(1,2)]
x1 <- data[data$Y == 1, c(1,2)]
symbols(x0, circles=rep(0.4, nrow(x0)), inches=FALSE, bg="brown")
symbols(x1, squares=rep(0.6, nrow(x1)), inches=FALSE, add=TRUE, bg="blue")
contour(x1vals, x2vals, zz, col="purple", add=T, levels=c(0.4))


# Assignment2: Adjust the ANN parameters to find the best possible solution(s)



# -------------------------------------------------------------------------------
# SVM Example

library(e1071)

?svm()

data <- read.csv('rawdata.txt', header=FALSE)
names(data) <- c("X1", "X2", "Y")

data[,3] <- as.factor(data[,3])
x0 <- data[data$Y == 0, c(1,2)]
x1 <- data[data$Y == 1, c(1, 2)]

model <- svm(Y~., data=data, cost=0.03, gamma=1, coef0=10, kernel="polynomial")

# Other parameters/kernels to try (make adjustments, optimize the solutions):
#model <- svm(Y~., data=data, cost=0.3, gamma=0.1)
#model <- svm(Y~., data=data, cost=1, gamma=0.03, coef0=0.001, kernel="sigmoid")

npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)

df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(model)))

preds <- predict(model,expand.grid(df))
zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)
dim(zz)

symbols(x0, circles=rep(0.4, nrow(x0)), inches=FALSE, bg="gold")
symbols(x1, squares=rep(0.6, nrow(x1)), inches=FALSE, add=TRUE, bg="purple")
contour(x1vals, x2vals, zz, col="blue", add=T, levels=1:3)

# Assignment3: Adjust the SVM parameters to find the best possible solution(s)
#              Use tune.svm to benchmark various solutions/kernels

?tune.svm()
# Note: tune.svm may run for several minutes .....
tunevals <- c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30)
tune_out <- tune.svm(Y~., data=data, kernel="polynomial", gamma=tunevals, cost=tunevals, coef0=tunevals)
tune_out


# Assignment4 : Choose 2 more learning algorithms to solve the problem and optimize the results


# -------------------------------------------------------------------------------
# tree 1st algorithm
library(rpart)

data <- read.csv('rawdata.txt', header=FALSE)
names(data) <- c("X1", "X2", "Y")

minsplit <- 5
maxdepth <- 10

data.rpart <- rpart(Y~., data=data, maxdepth=maxdepth, minsplit=minsplit)
npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)

df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(model)))

preds <- predict(data.rpart,expand.grid(df))
zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)
dim(zz)

symbols(x0, circles=rep(0.4, nrow(x0)), inches=FALSE, bg="gold")
symbols(x1, squares=rep(0.6, nrow(x1)), inches=FALSE, add=TRUE, bg="purple")
contour(x1vals, x2vals, zz, col="blue", add=T, levels=1:3)
plot(data.rpart)

#boosting 2nd algorithm
library(rpart)
library(caret)
library(lattice)
library(ggplot2)
library(adabag)
data <- read.csv('rawdata.txt', header=FALSE)
l <- length(data[,1])
sub <- sample(1:l,2*l/3)
data[,3] = as.factor(data[,3])
names(data) <- c("X1", "X2", "Y")
mfinal = 25
maxdepth = 10
data.boost <- boosting(Y~., data=data[sub,], boos = TRUE,  mfinal=mfinal, coeflearn = 'Breiman')
npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)
df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(model)))
plot(preds)
preds <- predict.boosting(data.boost, newdata=data[-sub,])
#zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)
dim(zz)

symbols(x0, circles=rep(0.4, nrow(x0)), inches=FALSE, bg="gold")
symbols(x1, squares=rep(0.6, nrow(x1)), inches=FALSE, add=TRUE, bg="purple")
contour(x1vals, x2vals, zz, col="blue", add=T, levels=1:3)

