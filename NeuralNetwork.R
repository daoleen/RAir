INITIAL_THETA_EPSILON1 <- sqrt(6) / sqrt(12);
INITIAL_THETA_EPSILON2 <- sqrt(6) / sqrt(10);
INITIAL_THETA_EPSILON3 <- sqrt(6) / sqrt(9);

# Random initialization the weights
Theta1 <- matrix(runif(5*8, -INITIAL_THETA_EPSILON1, INITIAL_THETA_EPSILON1), ncol=7, byrow=T)
Theta2 <- matrix(runif(5*8, -INITIAL_THETA_EPSILON2, INITIAL_THETA_EPSILON2), ncol=7, byrow=T)
Theta3 <- matrix(runif(4*8, -INITIAL_THETA_EPSILON3, INITIAL_THETA_EPSILON3), ncol=7, byrow=T)

ann.sigmoid <- function(z) {
  return(1 / (1 + exp(-z)))
}

ann.forwardPropagation <- function(X, Theta) {
  z1 <- X %*% aperm(Theta1)
  a2 <- ann.sigmoid(z1)
  z2 <- a2 %*% Theta2
  a3 <- ann.sigmoid(z2)
  z3 <- a3 %*% aperm(Theta3)
  a4 <- ann.sigmoid(z3)
  
  m <- length(X[,1])
}