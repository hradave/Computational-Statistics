RNGversion('3.5.1')

# Seed
set.seed(1234567890)


#### Question 3 ####

#1
myvar <- function(x) {
  n<-length(x)
  1/(n-1) * (sum(x^2) - sum(x)^2/n)
}


#2
set.seed(1234567890)
x <- rnorm(10000,mean=10^8,sd=1)


#3
Y <- vector(length = 10000)

for (i in 1:10000) {
  Y[i] <- myvar(x[1:i]) - var(x[1:i])
}

plot(x=1:10000, y=Y)


#Draw conclusions from this plot. How well does your function work? Can you explain the behaviour?




#4
myvar2 <- function(x) {
  n <- length(x)
  avg <- mean(x)
  s <- 0
  
  for (j in 1:n) {
    s <- s + (x[j]-avg)^2
  }
  
  return(s/(n-1))
  
}



Y <- vector(length = 10000)

for (i in 1:10000) {
  Y[i] <- myvar2(x[1:i]) - var(x[1:i])
}

plot(x=1:10000, y=Y, ylim=c(-0.0001,0.0001))



#### Question 4 ####

#1
data <- read.csv("tecator.csv", sep = ',')

#2
X <- as.matrix(data[,c(2:102,104)]) #215x102
y <- as.matrix(data[,103]) #215x1

A <- t(X) %*% X #102x102
b <- t(X) %*% y #102x1

#3
solve(a=A, b=b)
#system is computationally singular: reciprocal condition number = 7.13971e-17

#Matrix A in not invertible (which is what solve is trying to do)
det(A) #=0


#4
kappa(A)
#1.157834e+15

#Since A has no inverse, kappa(A) is using pseudo-inverse instead.

#The condition number gives a sense of how sensitive numerical results are to small errors in the input.
#A large condition number means the system will be more prone to small errors. 
#A small condition number means that small errors in the input will only lead to small errors in the output.

#Large kappa implies that the algorithm is numerically unstable.

# This kind of apparent ill-conditioning is called artificial ill-conditioning.
# It is due to the different rows (or columns) of the matrix having a very different
# scale; the condition number can be changed just by scaling the rows or
# columns. This usually does not make a linear system any better or any worse
# conditioned, but this fact emphasizes the importance of scaling in data analysis.
#(from the book p.208)

#5
data_scaled <- scale(data)

X_scaled <- as.matrix(data_scaled[,c(2:102,104)]) #215x102
y_scaled <- as.matrix(data_scaled[,103]) #215x1

A_scaled <- t(X_scaled) %*% X_scaled #102x102
b_scaled <- t(X_scaled) %*% y_scaled #102x1


solve(a=A_scaled, b=b_scaled)
#works
det(A_scaled) #why is this still 0?

kappa(A_scaled) #490471520662
