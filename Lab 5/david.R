############### QUESTION 1 #################

# 1

data <- read.csv2('lottery.csv')
plot(data$Day_of_year,data$Draft_No) #looks random
  
# 2

loess <- loess(Draft_No ~ Day_of_year,data = data)
Y_hat <- predict(loess, data)
plot(data$Day_of_year,data$Draft_No)
points(Y_hat)

# 3



# 4



# 5











############### QUESTION 2 #################


# 1



# 2



# 3



# 4




