library(rstan)
library(bayesplot)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num_dat <- read.csv(file.choose())
head(fish_num_dat)
str(fish_num_dat)
fish_num_dat$id <- as.factor(fish_num_dat$id)

# 計測していないデータを考慮しなければならない。このデータでいえば釣り人のスキルや道具など
# 無理やりポアソン回帰にあてはめてみる
glm_pois_brms <- brm(
  formula = fish_num ~ weather + temperature,
  family = poisson(),
  data = fish_num_dat,
  seed = 1,
  prior = set_prior("", class = "Intercept")
)
# 当てはめ値と95信頼区間の計算
set.seed(1)
eff_glm_pois <- conditional_effects(
  glm_pois_brms,
  method = "predict",
  effects = "temperature:weather",
  prob = 0.95
)
plot(eff_glm_pois, points = T)
# 予測範囲から大きく外れた値がある。過分散の状態
# 過分散はモデルよりも実際のデータがばらついている状態
# これに対処するのがGLMM（一般化線形混合モデル）

# mcmcに投入するデータを作る
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_dat)
sunny_dummy <- as.numeric(design_mat[, "weathersunny"])

dat_list <- list(
  N = nrow(fish_num_dat),
  fish_num = fish_num_dat$fish_num,
  temp = fish_num_dat$temperature,
  sunny = sunny_dummy
)

glmm_pois_stan <- stan(
  file = "4_1_glmm.stan",
  data = dat_list,
  seed = 1
)
# 収束の確認。推定パラメータが膨大なので一気に図示
mcmc_rhat(rhat(glmm_pois_stan))
# mcmcの結果表示
print(glmm_pois_stan,
      pars = c("Intercept", "b_sunny", "b_temp", "sigma_r"), #表示するパラメタの指定
      probs = c(0.025, 0.5, 0.975))

# 上記のモデル化をbrmsで実装する
# ランダム効果は、(1|id)とし、左側は切片、右側はランダム効果のグループ単位を表す
glmm_pois_brms <- brm(
  formula = fish_num ~ weather + temperature + (1|id), # ランダム効果の表記法
  family = poisson(),
  data = fish_num_dat,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sd"))
)
glmm_pois_brms
# ランダム効果はN(0,1)からえられた乱数に標準偏差をかけた値としている



