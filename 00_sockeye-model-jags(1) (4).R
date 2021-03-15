model {

  # Likelihood
  for (i in 1:I) {
    y[i] ~ dnorm(mu[i], tau) 
    
    mu[i] <- b0 + b1 * x1[i]
  }
  
  # Priors
  b0 ~ dnorm(55, 0.04) # equivalent to Normal(55, 5)
  b1 ~ dnorm(0, 0.01) # equivalent to Normal(0, 10)
  
  sig ~ dunif(0, 10)
  tau <- pow(sig, -2)
  
  # Quantity for predictive check
  for (i in 1:I) {
    y_rep[i] ~ dnorm(mu[i], tau)
    res[i] <- y[i] - y_rep[i]
  }
}