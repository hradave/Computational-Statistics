############### Question 1: Computations with Metropolis-Hastings ######################

# R version
RNGversion('3.5.1')

# Seed
set.seed(1234567890)

#libraries
library(coda)

# 1.

posterior <- function(x){
  x^5 * exp(-x)
}

posterior_sampler_ln <- function(tmax=1000, starting_point = 1) {
  
  
  X=rep(starting_point,tmax)
  
  for (t in 1:tmax) {
    
    Y = rlnorm(1,meanlog = X[t],sdlog = 1)
    U = runif(1,min = 0, max = 1)
    
    alpha = min(1,
                (posterior(Y) * dlnorm(X[t],meanlog = Y, sdlog = 1)) /
                  (posterior(X[t]) * dlnorm(Y, meanlog = X[t], sdlog = 1)))
    
    if (U < alpha) {
      X[t+1] = Y
    } else {
      X[t+1] = X[t]
    }
    
    t = t+1
    
  }
  
  return(X)
  
}

tmax=1000

# Seed
set.seed(1234567890)

plot(1:(tmax+1),posterior_sampler_ln(tmax = tmax), type ='l' 
     , main="Markov chain with log-normal proposal"
     , xlab = "time"
     , ylab = "X")


# 2.

posterior_sampler_chisq <- function(tmax=1000, starting_point = 1) {
  
  X=rep(starting_point,tmax)
  
  for (t in 1:tmax) {
    
    Y = rchisq(1,df = floor(X[t]+1))
    U = runif(1,min = 0, max = 1)
    
    alpha = min(1,
                (posterior(Y) * dchisq(X[t],df = floor(Y+1))) /
                  (posterior(X[t]) * dchisq(Y, df = floor(X[t]+1))))
    
    if (U < alpha) {
      X[t+1] = Y
    } else {
      X[t+1] = X[t]
    }
    
    t = t+1
    
  }
  
  return(X)
  
}

tmax=1000
X = posterior_sampler_chisq(starting_point = 1, tmax = tmax)
plot(1:(tmax+1),X, type = 'l')

# 3.

#conclusions

# 4.

starting_points = seq(1,10)


MCMC_seqs = mcmc.list()

for (k in starting_points) {
  MCMC_seqs[[k]] = as.mcmc(posterior_sampler_chisq(starting_point = k))
}

print(gelman.diag(MCMC_seqs))
# Potential scale reduction factors:
#   
#   Point est. Upper C.I.
# [1,]       1.02       1.03

# MCMC_seqs = mcmc.list()
# set.seed(1234567890)
# for (k in starting_points) {
#   MCMC_seqs[[k]] = as.mcmc(posterior_sampler_ln(starting_point = k))
# }
# 
# print(gelman.diag(MCMC_seqs))



#point estimate of the potential scale reduction factor is close to 1, so there is convergence.

# 5.

#integral is the Expected value of posterior distribution
#using Step1
X1 <- posterior_sampler_ln()
mean(X1) #larga variance

X2 <- posterior_sampler_chisq()
mean(X2) #small variance


# 6.

#Since the posterior distribution is a gamma distribution, its Expected value is alpha * beta,
#or in this case 6*1 = 6
