############# QUESTION 1 ##############
# R version
RNGversion('3.5.1')

#1

f <- function(x){
  return(x^2/exp(x) - 2*exp(-1*(9*sin(x)) / (x^2 + x + 1)))
}




#2

crossover <- function(x,y){
  return((x+y) / 2)
}


#3
mutate <- function(x){
  return(x^2 %% 30)
}

#4
genetic <- function(maxiter, mutprob){
  #a
  plot(x = seq(0,30), y = f(seq(0,30)), type = "l", xlab = "x", ylab = "f(x)")
  
  #b
  X = seq(0,30,5)
  
  #c
  Values = f(X)
  
  #d
  #set seed
  set.seed(1234567890)
  for (i in 1:maxiter) {
    #i
    parents = match(sample(X, 2),X)
    
    #ii
    victim = order(Values)[1]
    
    #iii
    kid = round(crossover(parents[1],parents[2]))
    p = runif(1)
    if (p < mutprob) {
      kid = mutate(kid)
    }
    
    #iv
    X[victim] = kid
    Values = f(X)
    
    #v
    max = max(Values)
  }
  
  #e
  print(X)
  print(Values)
  plot(x = seq(0,30), y = f(seq(0,30)), type = "l", xlab = "x", ylab = "f(x)")
  points(x = X, y = Values, col = "red", pch = 19)
}


