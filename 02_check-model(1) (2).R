# This code creates diagnostics for checking convergence of MCMC chains
# and identifiability of parameters

# Initial setup ----

# Packages tidyverse required here already loaded if you executed the command 
# in file 01_fit-model.R

library(MCMCvis)
library(bayesplot)
ns <- fit$mcmc.info$n.samples

# Check convergence of MCMC chains ----

pr <- matrix(c(rnorm(ns, 55, 5), rnorm(ns, 0, 10), runif(ns, 0, 10)), 
             nrow = ns, ncol = 3)

MCMCtrace(fit, params = c("b0", "b1", "sig"), pdf = FALSE, 
          priors = pr, Rhat = TRUE, post_zm = FALSE)

mcmc_acf(fit$samples, pars = c("b0", "b1", "sig"), lags = 10)


# Check model assumptions ----

df <- tibble(fitted = fit$mean$y_rep, resid = fit$mean$res)

## Fitted vs residual plot ----

ggplot(df, aes(x = fitted, y = resid)) +
  geom_point(colour = "#A6611A") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16, margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(size = 16, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text = element_text(size = 12))

## Normal Q-Q plot ----

ggplot(df, aes(sample = resid)) +
  stat_qq(colour = "#A6611A") + 
  stat_qq_line() +
  xlab("Residual quantiles") +
  ylab("Theoretical quantiles") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16, margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(size = 16, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text = element_text(size = 12))

## Histogram of residuals ----

ggplot(df, aes(x = resid)) +
  geom_histogram(aes(y=..density..), binwidth = 0.5, 
                 colour="black", fill="white") +
  geom_density(alpha=0.2, colour = "#A6611A", fill="#A6611A") +
  xlab("Residuals") +
  ylab("Density") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16, margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(size = 16, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text = element_text(size = 12))

# Predictive checks ----

ppc_dens_overlay(dat_jags$y, fit$sims.list$y_rep[1:250, ])
ppc_stat(dat_jags$y, fit$sims.list$y_rep[1:250, ], stat = "mean")
ppc_stat(dat_jags$y, fit$sims.list$y_rep[1:250, ], stat = "min")
ppc_stat(dat_jags$y, fit$sims.list$y_rep[1:250, ], stat = "max")
