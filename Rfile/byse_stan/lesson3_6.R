library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

weather <- read.csv(file.choose())
head(weather)
summary(weather)

# バイオリンプロット
ggplot(data = weather, mapping = aes(x = weather, y = sales))+
  geom_violin()+
  geom_point(aes(color = weather)) +
  labs(title = "sales beers by weather")

# 分散分析モデルを作る、ダミー変数を用いたGLM
anova_brms <- brm(
  formula = sales ~ weather,
  family = gaussian(),
  data = weather,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
anova_brms

# 推定された天気別の平均売上グラフ
eff <- conditional_effects(anova_brms) # marginal_effectsは非推奨になってた
plot(eff, points = FALSE)

