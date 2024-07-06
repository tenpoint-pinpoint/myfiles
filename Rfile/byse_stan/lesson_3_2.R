library(rstan)
library(bayesplot)
library(tidyverse)

#計算高速化
rstan_options(auto_write = TRUE) # モデルの自動保存
options(mc.cores = parallel::detectCores()) # 並列処理で高速化

# ファイルの読み込み
beers <- read_csv(file.choose())
head(beers, n= 3)
n <- nrow(beers)

# 描画
ggplot(beers, aes(x = temperature, y = sales))+
  geom_point()+
  labs(title = "sales-temperature")

# mcmcの実行
dat_list <- list(N = n, sales = beers$sales, temperature = beers$temperature)

mcmc_result <- stan(file = "3_2_beers.stan"
                    ,data = dat_list
                    ,seed = 1)
print(mcmc_result)

# サンプル抽出
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)

# トレースプロットと事後分布
mcmc_combo(
  mcmc_sample
  ,pars = c("Intercept", "beta", "sigma")
)
