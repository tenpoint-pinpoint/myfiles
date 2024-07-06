library(rstan)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

beers <- read.csv(file.choose())
sample_size <- nrow(beers)

# デザイン行列の作成
formula_lm <- formula(sales ~ temperature) # 目的変数~説明変数
X <- model.matrix(formula_lm, beers)

# データを作りmcmcのためlistにまとめる
N <- nrow(beers)
K <- 2
Y <- beers$sales
dat_list <- list(N=N,  K=K, Y=Y, X=X)

mcmc_result <- stan(
  file = "3_4_beers.stan"
  ,data = dat_list
  ,seed = 1
)

print(mcmc_result)
