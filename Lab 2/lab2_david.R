# Question 1: Optimizing a model parameter
# R version
RNGversion('3.5.1')


### 1.
data <- read.csv2("mortality_rate.csv",sep=";")

data<-cbind(data,LMR=log(data$Rate))
n=dim(data)[1]
set.seed(123456)
id=sample(1:n,floor(n*0.5))
train=data[id,]
test=data[-id,]

### 2.

myMSE <- function(lambda, pars, iterCounter = FALSE) {
  X <- pars$X
  Y <- pars$Y
  Xtest <- pars$Xtest
  Ytest <- pars$Ytest
  
  model <- loess(formula = Y~X, enp.target = lambda)
  
  Xtest_pred <- predict(model, newdata = Xtest)
  
  MSE <- sum((Ytest - Xtest_pred)^2) / length(Ytest)
  
  ####### THE FOLLOWING CODE REGARDING THE ITERATION COUNTER WAS WRITTEN BY Arian Barakat AND COPIED FROM
  #THE FILE FunctionCounter_Using_Environments_in_R.pdf
  
  # If we want a iteration counter
  if(iterCounter){
    if(!exists("iterForMyMSE")){
      # Control if the variable exists in the global environemnt,
      # if not, create a variable and set the value to 1. This
      # would be the case for the first iteration
      # We will call the variable 'iterForMyMSE'
      assign("iterForMyMSE",
             value = 1,
             globalenv())
    } else {
      # This part is for the 2nd and the subsequent iterations.
      # Starting of with obtaining the current iteration number
      # and then overwrite the current value by the incremental
      # increase of the current value
      currentNr <- get("iterForMyMSE")
      assign("iterForMyMSE",
             value = currentNr + 1,
             globalenv())
    }
  }

  return(MSE)
}

### 3.

pars <- list(X=as.matrix(train$Day), Y = as.matrix(train$LMR), Xtest = as.matrix(test$Day), Ytest = as.matrix(test$LMR))
lambda <- seq(from = 0.1, to = 40, by = 0.1)
MSE <- vector()

for (i in 1:length(lambda)) {
  MSE[i] <- myMSE(lambda = lambda[i], pars = pars, iterCounter = TRUE)
}

#result
MSE

### 4.

plot(lambda, MSE, pch = 20, col = "blue")
min_lambdas <- cbind(lambda, MSE)[which(MSE==min(MSE))] #lambda = 11.7; MSE = 0.131047
which(MSE==min(MSE))[1] #myMSE() evaluations required: 117

### 5.

#remove iteration variable if already exists
if (exists("iterForMyMSE")) {
  rm("iterForMyMSE")
}

MSE_min <- optimize(myMSE, interval = c(0.1, 40), tol = 0.01, pars = pars, iterCounter = TRUE)
iterForMyMSE #18 iterations
MSE_min$minimum
MSE_min$objective
#lambda = 10.69; MSE = 0.1321
#The function didn't manage to find the optimal MSE value, but it was close to it.


### 6.

MSE_min2 <- optim(method = "BFGS", par = 35, fn = myMSE, pars = pars)
MSE_min2$counts[1] #1 function call to myMSE()

#the starting from 35 was too high, the function only increases from the point. If we set it to a lower value,
#the optim() function does a better job.
#the optim() function doesn't move anywhere if the function is flat (gradient is 0)


################### WHAT ARE THE CONCLUSIONS? ############################################

# Question 2: Maximizing likelihood

### 1.
load("D:/LiU/Semester 2/Period 1/Computational Statistics/Computational-Statistics/Lab 2/data.RData")

### 2.
library(plotly)

#log-likelihood function:
#https://www.statlect.com/fundamentals-of-statistics/normal-distribution-maximum-likelihood

loglikelihood <- function(mu, sigma, data){
  n <- length(data)
  
  -n/2*log(2*pi) - n/2*log(sigma^2) - 1/(2*sigma^2)*sum((data - mu)^2)
}


##experimenting
mu_vec <- seq(-2,3,0.1)
sigma_vec <- seq(0.1,5.1, 0.1)

loglike_vec <- matrix(nrow=length(mu_vec), ncol=length(sigma_vec))

for (i in 1:length(mu_vec)) {
  for (j in 1:length(sigma_vec)) {
    loglike_vec[i,j] <- loglikelihood(mu_vec[i],sigma_vec[j],data)
    print(i)
    print(j)
    }
  }

plot(x=mu_vec,y=loglike_vec)
loglike_df <- data.frame(mu_vec,sigma_vec,loglike_vec)

plot_ly(z = as.matrix(loglike_vec)) %>% add_surface()

plot_ly(z = volcano, type = "surface")
### 3.

#https://www.statlect.com/glossary/log-likelihood

### 4.

