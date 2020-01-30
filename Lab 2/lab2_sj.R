#====================================================================
# COMPUTATIONAL STATISTICS - LAB 2
#====================================================================

# R version
RNGversion('3.5.1')

# Packages
library(tidyverse)

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

#


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

