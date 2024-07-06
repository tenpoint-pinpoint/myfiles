library(rstan)
library(bayesplot)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

beers <- read.csv(file.choose())
sample_size <- nrow(beers)

head(beers)

# 今回はモデルによる予測を行うので気温データを作っておく
temperature_pred <- 11:30
temperature_pred

# stanに入れるデータをリストにする
dat_list <-list(
  N = sample_size
  ,sales = beers$sales
  ,temperature = beers$temperature
  ,N_pred = length(temperature_pred)
  ,temperature_pred = temperature_pred
)

mcmc_result_pred <- stan(
  file = "3_3_beers.stan"
  ,data = dat_list
  ,seed = 1
)

#mcmcサンプルの抽出
mcmc_sample_pred <- rstan::extract(mcmc_result_pred, permuted = FALSE)

#気温11~30度の95%ベイズ予測区間を図示
mcmc_intervals(
  mcmc_sample_pred
  ,regex_pars = c("sales_pred.") # 正規表現で名称を指定
  ,prob = 0.8                    # 太い線の範囲
  ,prob_outer = 0.95             # 細い線の範囲
)

#mu_predの95%ベイズ予測区間の図示
mcmc_intervals(
  mcmc_sample_pred
  ,pars = c("mu_pred[1]", "sales_pred[1]")
  ,prob = 0.8
  ,prob_outer = 0.95
)

print(mcmc_result_pred)

#予測分布：気温が11度と30度の時
mcmc_areas(
  mcmc_sample_pred
  ,pars = c("sales_pred[1]", "sales_pred[20]")
  ,prob = 0.6
  ,prob_outer = 0.99
)


