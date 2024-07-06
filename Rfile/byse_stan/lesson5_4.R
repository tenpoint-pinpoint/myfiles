library(rstan)
library(brms)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding = "utf-8")

sales_df_2 <- read.csv(file.choose())
sales_df_2$date <- as.POSIXct(sales_df_2$date)
head(sales_df_2)
autoplot(ts(sales_df_2)[,-1])

### 通常の単回帰モデル
## 全期間
mod_lm <- brm(
  formula = sales ~ publicity,
  family = gaussian(link = "identity"),
  data = sales_df_2,
  seed = 1
)
fixef(mod_lm) # 、固定効果の係数だけを表示

## 期間を２つに分ける

sales_df_head <- head(sales_df_2, n = 50)
sales_df_tail <- tail(sales_df_2, n = 50)

mod_lm_head <- brm(
  formula = sales ~ publicity,
  family = gaussian(link = "identity"),
  data = sales_df_head,
  seed = 1
)
mod_lm_tail <- brm(
  formula = sales ~ publicity,
  family = gaussian(link = "identity"),
  data = sales_df_tail,
  seed = 1
)
fixef(mod_lm_head)
fixef(mod_lm_tail)
# 2つの期間で係数にかなりの差がある。これを１つの係数で考えるのは無理がある

dat_list <- list(
  T = nrow(sales_df_2),
  ex = sales_df_2$publicity,
  y = sales_df_2$sales
)

time_varying_stan <- stan(
  file = "5_4_time_varying.stan",
  data = dat_list,
  seed = 1,
  #chains = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
# 収束の確認 -> 問題なさそう
mcmc_rhat(rhat(time_varying_stan))
print(time_varying_stan,
      pars = c("s_w", "s_t", "s_v", "b[1]", "b[100]"),
      probs = c(0.025, 0.5, 0.975))
## 推定された状態の図示
mcmc_sample <- rstan::extract(time_varying_stan)
p_all <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = sales_df_2$date,
  obs_vec = sales_df_2$sales,
  state_name = "alpha",       # 状態方程式の結果をみたい
  graph_title = "est:state",
  y_label = "sales"
)
p_mu <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = sales_df_2$date,
  obs_vec = sales_df_2$sales,
  state_name = "mu",       # 状態方程式の結果をみたい
  graph_title = "est:mu(except publicity)",
  y_label = "sales"
)
p_b <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = sales_df_2$date,
  obs_vec = sales_df_2$sales, # 係数の値が欲しいので観測値は不要
  state_name = "b",           # 状態方程式の結果をみたい
  graph_title = "est:b",
  y_label = "coef"            #  係数を見ているのでcoef
)

grid.arrange(p_all, p_mu, p_b)
# 状態を示すalphaはギザギザしている
# 水準成分muは緩やかに上下を繰り返している
# 時変成分bは緩やかに減少している。宣伝効果が出なくなっている？



