install.packages("KFAS")
library(rstan)
library(bayesplot)
library(KFAS)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding = "utf-8")

data("boat")
boat
# 欠損値のことも考慮しつつstan実行
boat_omit_NA <- na.omit(as.numeric(boat))
head(boat_omit_NA)
length(boat)

dat_list <- list(
  T = length(boat),
  len_obs = length(boat_omit_NA),
  y = boat_omit_NA,
  obs_no = which(!is.na(boat))
)

dglm_binom <- stan(
  file = "5_8_dglm_binom.stan",
  data = dat_list,
  seed = 1,
  iter = 30000,
  warmup = 10000,
  thin = 20
)
# 収束の確認
mcmc_rhat(rhat(dglm_binom))
mcmc_sample <- rstan::extract(dglm_binom)

print(dglm_binom,
      pars = c("s_w", "lp__"),
      probs = c(0.025, 0.5, 0.975))

# 勝率の図示（yではなくpを描画）
years <- seq(from = as.POSIXct("1829-01-01"),
             by = "1 year",
             len = length(boat))
plotSSM(mcmc_sample = mcmc_sample,
        time_vec = years,
        obs_vec = as.numeric(boat),
        state_name = "probs",
        graph_title = "cembridge's win rate",
        y_label = "win rate",
        date_labels = "%Y"
        )
