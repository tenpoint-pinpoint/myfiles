library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding = "utf-8")

sales_df_4 <- read.csv(file.choose())
sales_df_4$date <- as.POSIXct(sales_df_4$date)
head(sales_df_4)
# まずは図示
autoplot(ts(sales_df_4)[,2])

# mcmcの実行
dat_list  <- list(
  T = nrow(sales_df_4),
  y = sales_df_4$sales
)

basic_srtucture <- stan(
  file = "5_6_basic_sts.stan",
  data = dat_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.97, max_treedepth = 15)
)
# 収束チェック
mcmc_rhat(rhat(basic_srtucture))

# 出力結果
print(basic_srtucture,
      pars = c("s_z", "s_s", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))
# 図示
mcmc_sample <- rstan::extract(basic_srtucture)

p_alpha <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = sales_df_4$date,
  obs_vec = sales_df_4$sales,
  state_name = "alpha",
  graph_title = "all state(mu+alpha) param",
  y_label = "sales"
)
p_trend <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = sales_df_4$date,
  obs_vec = sales_df_4$sales,
  state_name = "mu",
  graph_title = "trend(mu)param",
  y_label = "sales"
)
p_cycle <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = sales_df_4$date,,
  state_name = "gamma",
  graph_title = "cycle param",
  y_label = "gamma"
)

grid.arrange(p_alpha, p_trend, p_cycle)
