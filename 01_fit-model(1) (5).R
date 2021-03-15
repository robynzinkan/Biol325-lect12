# This code fits a linear regression model to the sockeye length data using
# Bayesian inference via JAGS

# Initial setup -----------------------------------------------------------

library(tidyverse)
library(jagsUI)

# Set up MCMC -------------------------------------------------------------

n.iter <- 10000
n.burnin <- 2000
n.thin <- 8
n.chains <- 2

# Process data ------------------------------------------------------------

dat <- read_csv("L13-Bayesian-Inference-II/sockeye_data.csv") %>%
  mutate(cnpgo = npgo - mean(npgo))

# JAGS requires data as a list
dat_jags <- list(y = dat$sle, x1 = dat$cnpgo, I = nrow(dat))

# Send to JAGS ------------------------------------------------------------

# Initial values

init <- function() {list(b0 = rnorm(1, 55, 10), b1 = rnorm(1, 0, 10),
                        sig = runif(1, 0, 10))}

# Parameters to monitor
pars <- c("b0", "b1", "sig", "y_rep", "res")

fit <- jagsUI(dat_jags, init, pars, 
              "L13-Bayesian-Inference-II/00_sockeye-model-jags.R", 
              n.chains = n.chains, n.iter = n.iter, 
              n.burnin = n.burnin, n.thin = n.thin,
              codaOnly = c("y_rep", "res"))

# Compare to lm() output
coef(lm(sle ~ cnpgo, data = dat))
sigma(lm(sle ~ cnpgo, data = dat))
