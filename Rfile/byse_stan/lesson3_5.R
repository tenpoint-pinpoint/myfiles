# brms = Bayesian Regression Model using Stan　の略
install.packages("brms")

library(rstan)
library(brms)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

beers <- read.csv(file.choose())

# brmsで単回帰モデル作成
simple_lm_brms <- brm(
  formula = sales ~ temperature         # 線形予測子
  ,family = gaussian(link = "identity") # 確率分布とリンク関数
  ,data = beers
  ,seed = 1
)
simple_lm_brms

# mcmcサンプルの取得
as.mcmc(simple_lm_brms, combine_chains = TRUE)
# トレースプロットと事後分布の描画
plot(simple_lm_brms)

### brmsの説明
# formulaは以下で外だし可能
s_formula <- bf(sales ~ temperature)
# 確率分布とリンク関数 <- 実行すると確率分布とリンク関数を表示
gaussian()
binomial()
poisson()
# brmで必要な設定を明示的に。以下はデフォルト
simple_lm_brms <- brm(
  formula = s_formula,
  family = gaussian(link = "identity"),
  data = beers,
  seed = 1,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  thin = 1
)
# 設定された事前分布を確認
prior_summary(simple_lm_brms)
# prior                  class        coef       group resp dpar nlpar lb ub
#(flat)                  b                                        
#(flat)                  b           temperature                            
#student_t(3, 71.5, 20)  Intercept                                        
#student_t(3, 0, 20)     sigma                                    0   

# propr列が事前分布 <- bは無情報事前分布、interceptとsigmaは弱情報事前分布
# class列がパラメータの役割
# coef列が関わる説明変数

# 事前分布を変更する
simple_lm_brms_3 <- brm(
  formula = sales ~ temperature,
  family  = gaussian(),
  data = beers,
  seed = 1,
  prior = prior(cauchy(0,10), class = "b")
)
simple_lm_brms_3
prior_summary(simple_lm_brms_3)

# 作成されたstanコード
stancode(simple_lm_brms_3)
# stanに渡したデータ
standata(simple_lm_brms_3)


# その他可視化 <- stanplotは非推奨になっているのでmcmc_plotを使う
mcmc_plot(simple_lm_brms_3,
         type = "intervals",
         variable = "^b_",
         regex = TRUE,
         prob = 0.8,   # 太い線の範囲
         prob_outer = 0.95 # 細い線の範囲
)


### brmsによる予測
# 予測のための説明変数
new_data <- data.frame(temperature = 20)
# 予測
fitted(simple_lm_brms_3, new_data)
fitted(simple_lm_brms, new_data)
# predict関数でもOK、こっちだと95%ベイズ予測区間が表示される
set.seed(1) # <- predictは内部でrnorm関数で乱数を生成しているため
predict(simple_lm_brms_3, new_data)



### predictを使わない予測
mcmc_sample <- as.mcmc(simple_lm_brms, combine_chains = TRUE)
head(mcmc_sample)
nrow(mcmc_sample)
# パラメータ別に保存
mcmc_intercept <- mcmc_sample[, "b_Intercept"]
mcmc_temperature <- mcmc_sample[,"b_temperature"]
mcmc_sigma <- mcmc_sample[,"sigma"]
# 気温20度の時
saigen_fitted <- mcmc_intercept + 20*mcmc_temperature # 20度の時の予測を4000個
mean(saigen_fitted) # fitの平均値
quantile(saigen_fitted, prob = c(0.025, 0.975)) # fitの四分位点
# fittedとの比較
fitted(simple_lm_brms, new_data)
# 予測分布を得るにはsigmaを考慮に入れる
set.seed(1)
saigen_predict <- do.call(
  rnorm,
  c(4000, list(mean = saigen_fitted, sd = mcmc_sigma))
)
mean(saigen_predict) # 予測の平均値
quantile(saigen_predict, prob = c(0.025, 0.975)) # 予測の四分位点
set.seed(1)
predict(simple_lm_brms, new_data)


### 回帰直線の図示 <- 青い太い線は事後中央値
eff <- marginal_effects(simple_lm_brms)
plot(eff, points = TRUE)

### 95％予測区間つきの回帰直線
set.seed(1)
eff_pre <- marginal_effects(simple_lm_brms, method = "predict")
plot(eff_pre, points = TRUE)

### 特定の説明変数だけを図示
marginal_effects(simple_lm_brms,
                 effects = "temperature", # 説明変数を指定
                 method = "predict")
