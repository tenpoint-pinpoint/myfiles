library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding = "utf-8")

fish_ts <- read.csv(file.choose())
fish_ts$date <- as.POSIXct(fish_ts$date)
head(fish_ts)

# まずは図示
autoplot(ts(fish_ts)[,-1])

# mcmcの実行
dat_list <- list(
  T = nrow(fish_ts),
  ex = fish_ts$temperature,
  y = fish_ts$fish_num
)

dglm_poisson <- stan(
  file = "5_9_dglm_poisson.stan",
  data = dat_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
# 収束チェック
mcmc_rhat(rhat(dglm_poisson))
# 結果表示
print(dglm_poisson,
      pars = c("s_z", "s_r", "b", "lp__"),
      probs = c(0.025, 0.5, 0.975))

# 描画
mcmc_sample <- rstan::extract(dglm_poisson)

p_all <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = fish_ts$date,
  obs_vec = fish_ts$fish_num,
  state_name = "lambda_exp",
  graph_title = "est_lambda",
  y_label = "fish_num",
  date_labels = "%Y-%m-%d"
)
p_smooth <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = fish_ts$date,
  obs_vec = fish_ts$fish_num,
  state_name = "lambda_smooth",
  graph_title = "est_lambda except random_effect",
  y_label = "fish_num",
  date_labels = "%Y-%m-%d"
)
p_fix <- plotSSM(
  mcmc_sample = mcmc_sample,
  time_vec = fish_ts$date,
  obs_vec = fish_ts$fish_num,
  state_name = "lambda_smooth_fix",
  graph_title = "est_lambda except random_effect and fix coef",
  y_label = "fish_num",
  date_labels = "%Y-%m-%d"
)

grid.arrange(p_all, p_smooth, p_fix)
