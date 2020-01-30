# Question 1: Optimizing a model parameter
# R version
RNGversion('3.5.1')


### 1.
data <- read.csv2("mortality_rate.csv",sep=";")

n=dim(data)[1]
set.seed(123456)
id=sample(1:n,floor(n*0.5))
train=data[id,]
test=data[-id,]

### 2.

### 3.

### 4.

### 5.

### 6.


# Question 2: Maximizing likelihood

### 1.
load("D:/LiU/Semester 2/Period 1/Computational Statistics/Computational-Statistics/Lab 2/data.RData")

### 2.

### 3.

### 4.

