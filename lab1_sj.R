#====================================================================
# COMPUTATIONAL STATISTICS - LAB 1
#====================================================================

library(ggplot2)
library(tidyverse)
library(matlib)
#--------------------------------------------------------------------
# QUESTION 1 - BE CAREFUL WHEN COMPARING
#--------------------------------------------------------------------

# Code snippet nr 1. 

x1 <- 1/3
x2 <- 1/4


if (x1-x2 == 1/12){
  print("Subtraction is correct") 
  }else{
    print("Subtraction is wrong")
  }

# Notes: Gives "Subtraction is wrong". 


# Code snippet nr 2. 

x1 <- 1
x2 <- 1/2


if (x1-x2 == 1/2){
  print("Subtraction is correct")
  }else{
  print("Subtraction is wrong")
}

# Notes: "Subtraction is correct".


# 1. Check the results of the snippets. Comment what is going on.


# Comments: Nr 1 and nr 2 should both give Subtraction is correct according to mathematical arithmetics. 
# It can be difficult to compare floats. Nr 1 is a rational number and can only be expressed as a 
# fraction of two integers to be precise, and it can only approximately be expressed in decimal form. 

# Since there is a problem when comparing floats, we should consider other solutions to the problem.



# Code snippet nr 1 with improvement
# Use isTRUE(), which handles the case when not true in `if` expressions. 
# all.equal(), tests if two objects are nearly equal, that is testing near equality.

x1 <- 1/3
x2 <- 1/4


if (isTRUE(all.equal(x1-x2,1/12))){
  print("Subtraction is correct") 
}else{
  print("Subtraction is wrong")
}


#--------------------------------------------------------------------
# QUESTION 2 - DERIVATIVE
#--------------------------------------------------------------------


f <- function(x) x

epsilon <- 1e-15
x<- 100
f(x+epsilon) - f(x)
derivative <- function(f, x, e){
  (f(x+epsilon) - f(x))/epsilon
} 

derivative(f, 0.999, epsilon) #1.11
derivative(f, 1, epsilon) #1.11
derivative(f,100000, epsilon) #0

# What values did you obtain? What are the true values? Explain the reasons behind the
# discovered differences.

# The derivative of a function at a particular point, will not affect this since 
# f(x)=x, f'(x)=1, so evaluating a point a such that f'(a), will still equal 1. 
# True values are 1. 
# However, 
# x=1 -> 1.1
# x=10000 -> 0

# This is due to the limitation of the storage in R. 
# In both cases we divide by a tiny number, i.e. epsilon.

# If we consider x within the range [0,1[, we obtain approximately 1. 
# The difference between large numbers, dominates the small epsilon, 
# which gives us zero in the denominator and thus the derivative evaluated at large points 
# is 0. 


#--------------------------------------------------------------------
# QUESTION 3 - VARIANCE
#--------------------------------------------------------------------

# Variance function
myvar <- function(x){
  n <- length(x)
  (1/(n-1)) * ( sum(x^2) - (1/n)*(sum(x))^2)
}

# Generate random numbers
x <- rnorm(10000, mean = 10^8 , sd = 1)

# Difference between the different funtions for the variance, for each subset
Y <- c()
for (i in seq_along(x)){
  Y[i] <- myvar(x[1:i]) - var(x[1:i])
}

# Create data frame
df <- data.frame(i = seq_along(x), y)

# Plot the results
ggplot(df, aes(x = i, y=y)) +
  geom_point() 

# Second implementation of the variance
myvar2 <- function(x){
  mean((x-mean(x))^2)
}

# Difference between the different funtions for the variance, for each subset
y2 <- c()
for (i in seq_along(x)){
  y2[i] <- myvar2(x[1:i]) - var(x[1:i])
}

# Create data frame
df2 <- data.frame(i=seq_along(x), y2)

# Plot the results
ggplot(df2, aes(x=i, y=y2)) +
  geom_point()


#--------------------------------------------------------------------
# QUESTION 4 - LINEAR ALGEBRA
#--------------------------------------------------------------------

# Read the data
tecator <- read.csv(file="tecator.csv")


y <- tecator$Protein
X <- tecator %>%
  select(-c(Sample, Protein)) %>%
  as.matrix()

# Computing A and B by matrix multiplication 
A <- t(X)%*%X
b <- t(X)*y


# Solving the linear system
solve(A,b) # Resukts in a singular design matrix

# Check the condition numbers
kappa(A) # high condition number indicates bad sign of solving the linear system 

# Improve the results by scaling
X <- tecator %>%
  select(-c(Sample, Protein)) %>%
  as.matrix() %>%
  scale()

y <- as.vector(scale(tecator$Protein))

# Solving the linear system
A <- t(X)%*%X
b <- t(X)*y

solve(A,b) # works

det(A)
# Check the condition numbers
kappa(A) # high condition number indicates bad sign of solving the linear system 
