install.packages("rstan")
library(rstan)

d <- list(X = 28, N = 50)
stanmodel <- stan_model(file='binom.stan')
