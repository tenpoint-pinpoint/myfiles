library(rstan)
library(brms)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num <- read.csv(file.choose())
head(fish_num)
summary(fish_num)

# 図示
ggplot(data = fish_num, mapping = aes(x = temperature,  y= fish_num))+
  geom_point(aes(color = weather))+ 
  labs(title = "fish, tenperature, weather")

# ポアソン回帰モデル
glm_pois <- brm(
  formula = fish_num ~ weather + temperature,
  family = poisson(),
  data = fish_num,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)

glm_pois

### モデルの解釈
# リンク関数が対数関数なので係数の解釈には要注意
# 出力結果     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept       -0.78      0.23    -1.25    -0.33 1.00     2667     2348
# weathersunny    -0.60      0.17    -0.93    -0.28 1.00     2763     2437
# temperature      0.08      0.01     0.06     0.10 1.00     2849     2663

# weathersunnyの場合、係数は-0.6。これは晴れると釣獲尾数はexp(-0.6)倍になるという意味
exp(-0.6) # 0.548816
# temperature、係数は0.08。これは晴れると釣獲尾数はexp(0.08)倍になるという意味
exp(0.08) # 1.083287

### 回帰曲線の図示
eff <- conditional_effects(glm_pois, effects = "temperature:weather")
plot(eff, points = TRUE)
# 気温が上がるほど指数的に増加していることがわかる

### 予測区間の図示 : 過分散（実際のデータの分散がモデルよりも大きくなる）
# 予測区間を描画することで過分散をチェックできる
set.seed(1)
eff_pre <- conditional_effects(glm_pois,
                               method = "predict",
                               effects = "temperature:weather",
                               prob = 0.99)
plot(eff_pre, points = TRUE)
# データが予測範囲外に多くあれば過分散である。問題なさそう







