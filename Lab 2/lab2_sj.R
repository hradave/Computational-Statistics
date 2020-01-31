#====================================================================
# COMPUTATIONAL STATISTICS - LAB 2
#====================================================================

# R version
RNGversion('3.5.1')

# Packages
library(tidyverse)
library(ggplot2)

#--------------------------------------------------------------------
# QUESTION 1 - Optimizing a model parameter
#--------------------------------------------------------------------

# 1.1 
# Import the mortality data set
data <- read.csv2("mortality_rate.csv")


# Add a new variable LMR
data <- data %>%
  mutate(LMR = log(Rate))

# Split data into training and test set
n = dim(data)[1]
set.seed(123456)
id = sample(1:n, floor(n * 0.5))
train = data[id ,]
test = data[-id ,]

# 1.2

# MSE function
myMSE <- function(lambda, pars) {
  X <- pars$X
  Y <- pars$Y
  Xtest <- pars$Xtest
  Ytest <- pars$Ytest
  model <- loess(Y ~ X, enp.target = lambda)
  fYpred <- predict(model, newdata = Xtest)
  MSE <- sum((Ytest - fYpred)^2) / length(Ytest)
  return(MSE)
}

# 1.3


pars <- list(X = as.matrix(train$Day),
             Y = as.matrix(train$LMR),
             Xtest = as.matrix(test$Day),
             Ytest = as.matrix(test$LMR))

# Lambda values [0.1,40]
lambda <- seq(from = 0.1, to = 40, by = 0.1)

# Estimate the predictive MSE values
MSE <- sapply(lambda, myMSE, pars)


# 1.4
# Plot lambda vs MSE
df_plot <- data.frame(lambda, MSE)
ggplot(df_plot, aes(x = lambda, y = MSE)) +
  geom_point(col = "darkblue") + 
  xlab(expression(lambda)) + 
  geom_vline(xintercept = 11.7)  # optimal lambda


lambda_opt <- cbind(lambda, MSE)[which(MSE == min(MSE))]
which(MSE == min(MSE))[1] 
#MSE evaluations required to find optimal is 117

### 5.

MSE_min <- optimize(myMSE, interval = c(0.1, 40), tol = 0.01, pars = pars) #lambda = 10.69; MSE = 0.1321
#The function didn't manage to find the optimal MSE value.
# If we have optimized over a narrower interval of values, then the algorithm would be able to find the 
# optimal value of MSE. But since the selected interval is a bad guess of where the optimal lamda is,
# it does not find it. The function optimize() is for one dimensional optimazation and it seraches the given interval
# for the optimum (min or max) w.r.t. its first argument. 

#################### HOW MANY TIME DID OPTIMIZE() CALL THE FUNCTION MYMSE() ##################################

### 6.

# Now we use the function optim which in contrast to the optimize function, is for general purpose optimazation,
# with the setting BFGS as the chosen method. The optim function does not find the optimal value when the stating point
# lambda = 35. This is a pretty bad guess if we look at the plot. However if we pick a better guess as the starting
# value, then it performs better. 

MSE_min2 <- optim(method = "BFGS", par = 35, fn = myMSE, pars = pars)
MSE_min2$counts[1] 

# I requres one function evaluation to find 


#--------------------------------------------------------------------
# QUESTION 2 - Maximizing likelihood
#--------------------------------------------------------------------

# 2.1
# Load the data
# Sample from normal distribution with parameters \mu and \sigma
load("data.Rdata")
x <- data


# 2.2
# Derived formulas for the MLE 

mu_ml <- function(x){
  mean(x)
}

sigma_ml <- function(x){
  term <- (x - mean(x))^2
  sigma <- sum(term)/length(x)
  return(sqrt(sigma))
}

# Value of MLE
mu <- mu_ml(x)
sigma <- sigma_ml(x)

# 2.3
# Minus log–likelihood function
minus_logL <- function(x, par){
  mu <- par[1]
  sigma <- par[2]
  (length(x)/2)*log(2*pi*sigma^2) + 1/(2*sigma^2) * sum((x - mu)^2)
} 

# Gradient (also minus)

gradient <- function(x, par){
  mu <- par[1]
  sigma <- par[2]
  -c((1/sigma)^2 * sum(x - mu),
     -(length(x)/sigma) + (1/sigma)^3 * sum((x - mu)^2))
}





# Optimize with initial parameters mu = 0, sigma = 1.


#--------------------------------------------------------------------
# Conjugate Gradient method
set.seed(123456)
# Start time
start.time <- Sys.time()

# With a finite-difference approximation
optim(par = c(0, 1), fn = minus_logL, gr = NULL, method = "CG", x = x)

# End time
end.time <- Sys.time()
time.taken <- end.time - start.time 

#--------------------------------------------------------------------
# Conjugate Gradient method
set.seed(123456)
# Start time
start.time <- Sys.time()

# With a specified gradient
optim(par = c(0, 1), fn = minus_logL, gr = gradient, method = "CG", x = x)

# End time
end.time <- Sys.time()
time.taken <- end.time - start.time 

#--------------------------------------------------------------------
# BFGS - With a finite-difference approximation
set.seed(123456)
# Start time
start.time <- Sys.time()

# With a finite-difference approximation
optim(par = c(0, 1), fn = minus_logL, gr = NULL, method = "BFGS", x = x)

# End time
end.time <- Sys.time()
time.taken <- end.time - start.time 

#--------------------------------------------------------------------
# BFGS - With a specified gradient
set.seed(123456)
# Start time
start.time <- Sys.time()

# Optimize
optim(par = c(0, 1), fn = minus_logL, gr = gradient, method = "BFGS", x = x)

# End time
end.time <- Sys.time()
time.taken <- end.time - start.time # 0.007583141 secs

