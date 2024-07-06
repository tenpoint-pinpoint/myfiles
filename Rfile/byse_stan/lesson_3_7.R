###　正規線形モデル
library(rstan)
library(brms)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sales_climate <- read.csv(file.choose())

summary(sales_climate)

# 売上と天気と気温の関係を可視化
ggplot(data = sales_climate, mapping = aes(x = temperature, y = sales))+
  geom_point(aes(color = weather))+
  labs(title = "sales, temperature, weather")

# 正規線形モデルの作成
lm_brms <- brm(
  formula = sales ~ weather +temperature,
  family = gaussian(),
  data = sales_climate,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms

# 95％ベイズ信用区間付きの回帰直線を図示
eff <- conditional_effects(lm_brms, effects = "temperature:weather")
plot(eff, points = TRUE)