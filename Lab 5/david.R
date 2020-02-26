############### QUESTION 1 #################

# 1
library(boot)

data <- read.csv2('lottery.csv')
plot(data$Day_of_year,data$Draft_No) #looks random
  
# 2

loess <- loess(Draft_No ~ Day_of_year,data = data)
Y_hat <- predict(loess, data)
plot(data$Day_of_year,data$Draft_No)
points(Y_hat)

# 3

loess <- loess(Draft_No ~ Day_of_year,data = data)
Y_hat <- predict(loess, data)
Xb <- strtoi(names(which(Y_hat == max(Y_hat))))
Xa <- strtoi(names(which(Y_hat == min(Y_hat))))


stat <- function(data, vn) {
  data_bs <- data[vn,]
  loess <- loess(Draft_No ~ Day_of_year,data = data_bs)
  Y_hat <- predict(loess, data_bs)
  
  Xb <- which.max(unname(Y_hat))
  Xa <- which.min(unname(Y_hat))
  
  t_stat <- (unname(Y_hat[Xb]) - unname(Y_hat[Xa])) / (Xb - Xa)
  
  return(t_stat)
  
}

res <- boot(data, stat, R = 2000)


# 4



# 5











############### QUESTION 2 #################


# 1



# 2



# 3



# 4



