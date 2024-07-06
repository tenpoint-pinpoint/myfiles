library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding = "utf-8")

sales_df_3 <- read.csv(file.choose())
sales_df_3$date <- as.POSIXct(sales_df_3$date)
head(sales_df_3)

# 図示
autoplot(ts(sales_df_3$sales))

## ローカルレベルモデルのmcmc実行
dat_list <- list(
  y = sales_df_3$sales,
  T = nrow(sales_df_3)
) 
local_level <- stan(
  file = "5_2_local_level.stan",
  data = dat_list,
  seed = 1
)
# 収束確認 -> 問題なさそう
mcmc_rhat(rhat(local_level))
# 結果出力
print(local_level,
      pars = c("s_w", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))

## 平滑化トレンドモデルの実行
smooth_trend <- stan(
  file = "5_5_smooth_trend.stan",
  data = dat_list,
  seed = 1,
  #chains = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)
# 収束の確認 -> 大丈夫そう
mcmc_rhat(rhat(smooth_trend))
#結果の出力
print(smooth_trend,
      pars = c("s_z", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975)
)

## ローカル線形トレンドモデルのmcmc実行
liner_smooth_trend <- stan(
  file = "5_5_liner_smooth.stan",
  data = dat_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
# 収束の確認
mcmc_rhat(rhat(liner_smooth_trend))
# 結果出力
print(liner_smooth_trend,
      pars = c("s_w", "s_v", "s_z", "lp__"),
      probs = c(0.025, 0.5, 0.975)
)

# サンプル取得
mcmc_sample_local <- rstan::extract(local_level)
mcmc_sample_smooth <- rstan::extract(smooth_trend)
mcmc_sample_liner_trend <- rstan::extract(liner_smooth_trend)

# 図示
p_local <- plotSSM(
  mcmc_sample = mcmc_sample_local,
  time_vec = sales_df_3$date,
  obs_vec = sales_df_3$sales,
  state_name = "mu",
  graph_title = "lolac_level",
  y_label = "sales"
)
p_smooth <- plotSSM(
  mcmc_sample = mcmc_sample_smooth,
  time_vec = sales_df_3$date,
  obs_vec = sales_df_3$sales,
  state_name = "mu",
  graph_title = "smooth_trend",
  y_label = "sales"
)
p_liler_trend <- plotSSM(
  mcmc_sample = mcmc_sample_liner_trend,
  time_vec = sales_df_3$date,
  obs_vec = sales_df_3$sales,
  state_name = "mu",
  graph_title = "local_liner_trend",
  y_label = "sales"
)
grid.arrange(p_local, p_smooth, p_liler_trend)


# ドリフト成分の可視化
plotSSM(mcmc_sample = mcmc_sample_liner_trend,
        time_vec = sales_df_3$date,
          state_name = "delta",
          graph_title = "drift",
          y_label = " delta"
)
  
)