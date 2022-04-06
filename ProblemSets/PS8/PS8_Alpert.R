library(nloptr)
library(tidyverse)
library(modelsummary)

set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1] <- 1 # first column of X should be all ones
eps <- rnorm(N,mean=0,sd=0.5)
betaTrue <- c (1.5, -1, -.25,.75, 3.5, -2, .5, 1, 1.25, 2)
Y <- X%*%betaTrue + eps

betahat <- solve(t(X)%*%X)%*%t(X)%*%Y



# set up a stepsize
alpha <- 0.0000003
# set up a number of iterations
maxiter <- 500000
# Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}
# define the gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

# initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# randomly initialize a value to beta
set.seed(100)
# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)
# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}
# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))


# Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}
# Gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(result)


# Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradient <- function(theta,Y,X {
  grad <-as.vector(rep(0,length(theta)))
  beta <-theta[1:(length(theta)-1)]
  sig <-theta[length(theta)]
  grad[1:(length(theta)-1)]<- -t(X)%*%(Y-X%*%beta)/(sig^2)
  grad[length(theta)] <-dim(X)[1]/sig - crossprod(Y-X%*%beta)/sig^3
  
  return (grad)
}

lm(Y~X -1)


