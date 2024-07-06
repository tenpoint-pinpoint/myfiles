library(rstan)
library(bayesplot)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num_dat <- read.csv(file.choose())
head(fish_num_dat)

# 交互作用組み込んだポアソン回帰モデル
glm_pois_brms <- brm(
  formula = fish_num ~ temperature * human,
  family = poisson(),
  data = fish_num_dat,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)
# 交互作用モデルの回帰曲線をかく
condition <- data.frame(
  human = unique(fish_num_dat$human)
)
plot(conditional_effects(glm_pois_brms,
                         effects = "temperature",
                         conditions = condition)
)

## ランダム係数モデルの推定
glmm_pois_brms <- brm(
  formula = fish_num ~ temperature + (temperature||human), # ランダム切片とランダム係数に相関を認めない
 #(||)の左側にランダム効果の与えられる相手、右がランダム効果のグループ
 #上記の記述でランダム切片+ランダム係数のモデルとなっている
 #formula = fish_num ~ tenperature + (tenperature|human)だとランダム切片とランダム係数に相関を認める
 family = poisson(),
 data = fish_num_dat,
 seed = 1,
 # 今回は弱情報事前分布を使う
 iter = 6000,   # 複数のランダム効果を想定する場合はiterを増やす 
 warmup = 5000,
 control =  list(adapt_delta = 0.97, max_treedepth = 15) # warningを避けるため
)
glmm_pois_brms

# 回帰曲線の図示
plot(conditional_effects(glmm_pois_brms,
                         re_formula = NULL,
                         effects = "temperature",
                         conditions = condition),
     points = T)
# 縮約効果によって、Jの回帰曲線が右肩上りになった