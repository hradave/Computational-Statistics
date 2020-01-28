# Computer Lab 2

# R version
RNGversion('3.5.1')

# Reading data
mortality <- read.csv2("Lab 2/mortality_rate.csv")
mortality$LMR <- log(mortality$Rate)

# Dividing data
n=dim(mortality) [ 1 ]
set.seed(123456)
id=sample(1:n, floor(n*0.5))
train=mortality[id,]
test=mortality[id,]


# 2
myMSE <- function(lambda, X, Y, Xtest, Ytest)
  {
    model <- loess(Y ~ X, enp.target = lambda)
    pred <- predict(model, newdata = Xtest)
    MSE <- (1/length(pred))*sum((Ytest - pred)^2)
    return(MSE)
  }


# 3
lambda_seq <- seq(from = 0.1, to = 40, by = 0.1)
predicted_MSE <- numeric()
for (i in lambda_seq) {
  predicted_MSE <- append(predicted_MSE, myMSE(lambda=i,
                                               X=as.matrix(train[,1:2]),
                                               Y=as.matrix(train[,3]),
                                               Xtest=as.matrix(test[,1:2]), 
                                               Ytest=as.matrix(test[,3])))
}


# 4
plot(lambda_seq, predicted_MSE, col = "blue", ylab = "MSE", xlab = "Lambda")


# 4 ????
lambda_model <- function(lambda)
{
  model <- loess(as.matrix(train[,3]) ~ as.matrix(train[,1:2]),
                 enp.target = lambda)
  pred <- predict(model, newdata = as.matrix(test[,1:2]))
  MSE <- (1/length(pred))*sum((as.matrix(test[,3]) - pred)^2)
  return(MSE)
}

#5
optimized <- optimize(lambda_model, 0.1:40, tol = 0.01)
optimized

  


