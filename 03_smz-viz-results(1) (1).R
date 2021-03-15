# This code creates summaries and visualizations of the results based on 
# samples of the posterior distribution

# Initial setup ----

# Packages tidyverse and MCMCvis and variable ns (number of samples) required 
# here already loaded if you executed the command in file 02_check-model.R

library(ggfan)

# Summary table ----

MCMCsummary(fit, round = 2, params = c("b0", "b1", "sig"))

# Summarize posterior for Beta 1 (slope for centre NPGO) ----

post_b1 <- tibble(b1 = density(fit$sims.list$b1)$x,
                  dens = density(fit$sims.list$b1)$y)

p_pos <- round(mean(fit$sims.list$b1 > 0), 2)
ci95 <- quantile(fit$sims.list$b1, prob = c(0.025, 0.975))

ggplot(post_b1, aes(x = b1, y = dens)) +
  geom_line() +
  annotate("text", x = 1, y = 1.75, size = 6,
           label = substitute(paste("P(", beta[1], "> 0) = ", p, sep = ""), 
                              list(p = p_pos))) +  
  geom_area(aes(x = ifelse(b1 > ci95[1] & b1 < ci95[2], b1, 0)),
            fill = "#018571", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0, max(post_b1$dens) * 1.1),
                     expand = c(0, 0)) +
  xlab(expression(beta[1])) +
  ylab("Posterior density") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16, margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(size = 16, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text = element_text(size = 12))

# Compute function of parameters ----

# Note: we will create a posterior distribution of the product between b0 and b1.
# This is not something we would do for the kind of model being evaluated here.
# The purpose is just to show you that we use samples from posterior 
# distributions of model parameters to create the posterior distribution for 
# other parameters or quantities that are a function of model parameters.

fpar <- fit$sims.list$b0 * fit$sims.list$b1 

post_fpar <- tibble(fpar = density(fpar)$x,
                  dens = density(fpar)$y)

p_pos <- round(mean(fpar > 0), 2)
ci95 <- quantile(fpar, prob = c(0.025, 0.975))

ggplot(post_fpar, aes(x = fpar, y = dens)) +
  geom_line() +
  annotate("text", x = 52.5, y = 0.01, size = 6,
           label = substitute(paste("P(", beta[0], beta[1], "> 0) = ", p, sep = ""), 
                              list(p = p_pos))) +  
  geom_area(aes(x = ifelse(fpar > ci95[1] & fpar < ci95[2], fpar, 0)),
            fill = "#018571", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0, max(post_fpar$dens) * 1.1),
                     expand = c(0, 0)) +
  xlab(expression(beta[0] * beta[1])) +
  ylab("Posterior density") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16, margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(size = 16, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text = element_text(size = 12))

# Fitted model plot ----

cnpgo <- seq(min(dat$cnpgo), max(dat$cnpgo), 0.05)

df <- tibble(b0 = fit$sims.list$b0,
             b1 = fit$sims.list$b1) %>%
  expand(nesting(b0, b1), cnpgo) %>%
  mutate(sle = b0 + b1 * cnpgo,
         npgo = cnpgo + mean(dat$npgo))

ggplot(df, aes(x = npgo, y = sle)) +
  geom_fan(show.legend = FALSE) +
  ggfan::geom_interval(intervals = c(0, 0.5, 0.95)) +
  geom_point(data = dat, aes(x = npgo, y = sle), colour = "#A6611A") +
  scale_fill_gradient(low = "#004529", high = "#f7fcb9") +
  scale_y_continuous(breaks = seq(49, 57, 1), limits = c(49, 57)) +
  xlab("North Pacific Gyre Oscillation Index") +
  ylab("Standard length (cm)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 16, margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(size = 16, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text = element_text(size = 12),
        legend.position = "top")
