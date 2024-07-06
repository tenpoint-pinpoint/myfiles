library(tidyverse)
library(rstan)
library(bayesplot)

rstan_options(auto_write = TRUE) 
# TRUEにしてMCMCを行うと、[.rds]拡張子のRDSファイルが生成される。２回目以降コンパイル不要になる
options(mc.cores = parallel::detectCores())
# 計算を並列化して計算処理速度UP 

beer_sales <- read.csv(file.choose())
summary(beer_sales)

# ビールの種類毎にヒストグラム
ggplot(data = beer_sales, mapping = aes(x = sales, y = ..density.., color = beer_name, fill =beer_name ))+
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)

# ビールの種類毎にデータをリスト化

sales_a <- beer_sales$sales[1:100]
sales_b <- beer_sales$sales[101:200]

dat_list <- list(sales_a = sales_a, sales_b = sales_b, N = 100)
dat_list


# mcmcの実行
mcmc_result <- stan(
  file = "2_6_diff.stan"
  ,data = dat_list
  ,seed = 1
)

print(mcmc_result, probs = c(0.025, 0.5, 0.975))

# 平均値の差の事後分布 :: 興味のある対象をgenerated quantitiesブロックで生成して吟味する
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)
mcmc_dens(mcmc_sample, pars = "diff")

mcmc_result
