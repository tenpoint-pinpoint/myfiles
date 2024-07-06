library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

interaction_1 <- read.csv(file.choose())
head(interaction_1)

### 交互作用
# 以下の２つはformula構文で同じ意味になる
# formula = sales ~ publicity * bargen
# formula = sales ~ publicity + bargen + publicity:bargen

## カテゴリカル*カテゴリカル
# まずデザイン行列を作る
model.matrix(sales ~ publicity * bargen, interaction_1)
# brmsモデルの作成
interaction_brms_01 <- brm(
  formula = sales ~ publicity * bargen,
  family = gaussian(),
  data = interaction_1,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

interaction_brms_01
# 係数の解釈
newdata01 <- data.frame(
  publicity = rep(c("not", "to_implement"), 2), # repは指定したデータを繰り返し取得
  bargen = rep(c("not","to_implement"), each = 2)
)

newdata01
# 予測
round(fitted(interaction_brms_01, newdata01), 2)
#       Estimate Est.Error   Q2.5  Q97.5
# [1,]   103.42      3.64   96.61 110.97 -> ×、× 
# [2,]   113.34      3.77  106.06 120.51 -> ○、× 
# [3,]   130.63      3.68  123.31 137.80 -> ×、○
# [4,]   161.40      3.69  154.05 168.56 -> ○、○

# 図示
eff_1 <- conditional_effects(interaction_brms_01,
                             effects = "publicity:bargen")
plot(eff_1, points = T)

## カテゴリカル*数量
interaction_2 <- read.csv(file.choose())
head(interaction_2)
# モデル化
interaction_brms_02 <- brm(
  formula = sales ~ publicity * temperature,
  family = gaussian(link = "identity"),
  data = interaction_2,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
) 

interaction_brms_02
# 係数の解釈
# 説明変数を新たに作る <- 新しい状況のデータにモデルを当てはめる
newdata02 <- data.frame(
  publicity = rep(c("not", "to_implement"), each = 2),
  temperature = c(0,10,0,10)
)
newdata02
# 予測
round(fitted(interaction_brms_02, newdata02),2)
# 図示
eff_2 <- conditional_effects(interaction_brms_02,
                             effects = "temperature:publicity")
plot(eff_2, points = T)


## 数量*数量
interaction_3 <- read.csv(file.choose())
head(interaction_3)

# まず可視化
ggplot(data = interaction_3, mapping = aes(x = product, y = sales, color = factor(clerk)))+
  geom_point()

# モデル作成
interaction_brms_03 <- brm(
  formula = sales ~ product * clerk,
  family = gaussian(link = "identity"),
  data = interaction_3,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
# 使われた事前分布をチェック
interaction_brms_03
stancode(interaction_brms_03)
# 係数の解釈
newdata03 <- data.frame(
  product = c(0,10,0,10),
  clerk = c(0,0,10,10)
)
# 予測（モデルへの当てはめ値）
round(fitted(interaction_brms_03, newdata03),2)
# モデルの図示
# 1,複数の回帰直線を引く方法
int_conditions <- list(
  clerk = setNames(1:9, paste("clerk=", 1:9, sep="")) # setNamesは数値に名前を持つべくろつの作成関数
)
int_conditions

eff_3_1 <- conditional_effects(interaction_brms_03,
                               effects = "product:clerk",
                               int_conditions = int_conditions)
plot(eff_3_1, points = T)  
  
# 2,グラフ行列を描く
conditions <- data.frame(clerk = 1:9)
eff_3_2 <- conditional_effects(interaction_brms_03,
                               effects = "product",
                               conditions = conditions) 
plot(eff_3_2, points = F)



