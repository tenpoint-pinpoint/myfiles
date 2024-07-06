library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat <- read.csv(file.choose())
head(dat,3)
summary(dat)

# 可視化
ggplot(data = dat, mapping = aes(x=nutrition, y= germination, color = solar))+
  geom_point()+
  labs(title = "germination")
# 栄養量に比例して発芽率高い、日照がある方が発芽率高い

#brmsで推定
glm_bino_brms <- brm(
  formula = germination | trials(size) ~ solar + nutrition, # | trials()はbrms上の表記ルール
  family = binomial(), # リンク関数：二項分布のデフォルトはロジット
  data = dat,
  seed = 1,
  prior = c(set_prior("", class = "Intercept")) # 切片の事前分布は無情報
)
glm_bino_brms

### モデルの解釈
# 出力結果
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept        -8.01      0.51    -9.06    -7.06 1.00     1471     1545
# solarsunshine     4.04      0.29     3.50     4.62 1.00     1803     1951
# nutrition         0.72      0.05     0.62     0.83 1.00     1737     1944

# オッズ = p / 1-p ...失敗するより何倍成功しやすいかの指標
# オッズの変化率をオッズ比、その対数を対数オッズ比という
# ロジスティック回帰モデルの係数にexpを適用するとオッズ比になる

## 実験
# 説明変数を作る
new_dat <- data.frame(solar = c("shade", "sunshine", "sunshine"),
                      nutrition = c(2, 2, 3),
                      size = c(10, 10, 10))
# brmsの結果を用いて予測する
# 線形予測子を使ってpiを計算
linear_fit <- fitted(glm_bino_brms, new_dat, scale = "linear")[,1]
# ロジスティック関数< 1/1+exp(-pi)>を使ってを計算し、成功確率に変換
fit  <- 1 / (1 + exp(-linear_fit)) 
fit # 成功確率

# オッズを計算する -> 成功確率をpとしてロジット関数をかませる
odds1 <-  fit[1] / (1 - fit[1])  # 曇り、栄養2
odds2 <-  fit[2] / (1 - fit[2])  # 晴れ、栄養2
odds3 <-  fit[3] / (1 - fit[3])  # 晴れ、栄養3

# モデルの係数を取得しておく
coef <- fixef(glm_bino_brms)[,1]
coef

# solarがshadeからsunshineに変わった時のオッズ比は、solarの係数にexpをとったもの
odds_rate <- exp(coef[2])
odds_rate
# 上記と等しくなるオッズ比は odds1 / odds2 ... 曇りと晴れの差
odds_rate1_2 <- odds2 / odds1
odds_rate1_2

# nutritionが２から３に変わった時のオッズ比
odds_rate_n <- exp(coef[3])
odds_rate_n1 <- odds3/ odds2
odds_rate_n
odds_rate_n1

# 回帰曲線の図示
eff <- conditional_effects(glm_bino_brms,
                           conditions = data.frame(size = 10), # 試行回数を設定
                           effects = "nutrition:solar")
plot(eff, points = TRUE)
