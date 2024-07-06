library(rstan)
library(bayesplot)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num_dat <- read.csv(file.choose())

### ランダム切片モデル
## グループ毎に異なるランダム効果を与える
head(fish_num_dat)

# モデル化
glmm_pois_brms_g <- brm(
  formula = fish_num ~ weather + temperature + (1|human),
  family = poisson(),
  data = fish_num_dat,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sd"))
)
# 収束を確認する
plot(glmm_pois_brms_g)
mcmc_plot(glmm_pois_brms_g)
# 結果の表示
glmm_pois_brms_g
# 各グループでのランダム効果の影響の大きさをみる
ranef(glmm_pois_brms_g)

## 回帰曲線の図示
condition <- data.frame(
  human = unique(fish_num_dat$human)
)

eff_glmm_g <- conditional_effects(
  glmm_pois_brms_g,
  effects = "temperature:weather",
  re_formula = NULL, # ランダム効果を図示するためformulaを変更しないようにする
  conditions = condition
)

plot(eff_glmm_g, points = T)
