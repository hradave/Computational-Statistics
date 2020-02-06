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
  rand <- round(runif(1,min=1, max=sum(data$Population)),0)
  
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
#the histograms look similar, but more populated cities appear more often.




################################## checking the algorithm with no weights ###########################
set.seed(1234567890)

sampler2 <- function(data) {
  rand <- round(runif(1,min=1, max=dim(data)[1]),0)
  return(data[rand,1])
}
selected_cities2 <- vector(length = 20, mode = "character")
cities2 <- data

set.seed(1234567890)
for (j in 1:20) {
  selected_cities2[j] <- sampler2(cities2)
  cities2 <- cities2[-which(cities2$Municipality==selected_cities2[j]),]
}

sample2 <- data[which(data$Municipality %in% selected_cities2),]
mean(sample2$Population) #41792.15
hist(sample2$Population, breaks = 50, xlim=c(0,8e+05))
