################################## Question 1: Cluster sampling ##################################
# R version
RNGversion('3.5.1')

#1

#https://medium.com/@peterkellyonline/weighted-random-selection-3ff222917eb6

data <- read.csv2("population.csv", stringsAsFactors = FALSE)


#2
# Seed
set.seed(1234567890)

sampler <- function(data) {
  rand <- round(runif(1,min=1, max=sum(data$Population)),1)
  
  for (i in 1:dim(data)[1]) {
    rand <- rand - data[i,2]
    if (rand <= 0) {
      return(data[i,1])
    }
  }
  
}

#3
selected_cities <- vector(length = 20, mode = "character")
cities <- data

set.seed(1234567890)
for (j in 1:20) {
  selected_cities[j] <- sampler(cities)
  cities <- cities[-which(cities$Municipality==selected_cities[j]),]
}


#4
#the selected cities:
sample <- data[which(data$Municipality %in% selected_cities),]
mean(sample$Population) #94273.3
mean(data$Population) #32209.25
#The size of the selected cities are a bit different, but they are of the same magnitude.


#5
hist(data$Population, breaks = 50)
hist(sample$Population, breaks = 50)
#the histograms look the same
