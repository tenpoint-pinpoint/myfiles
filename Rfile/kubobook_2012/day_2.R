
dat_2 <- read.csv("./poisson/data3a.csv")
dat_2$f <- as.factor(dat_2$f)

str(dat_2)

# 散布図
plot(dat_2$x, dat_2$y, pch = c(21,19)[dat_2$f])
legend("topleft", legend = c("C", "T"), pch = c(21,19))

# ポアソン回帰による推定
# xで説明　　 λ = log(β0 + β1x)　というモデル
fit <- glm(y~x, data = dat_2, family = poisson) #yをxで推定、familyで利用する分布を指定している
fit 
# 出力される指標
# 最尤推定値     Coefficients  
# 自由度         Degrees of Freedom
# null逸脱度     Null Deviance
# 残差逸脱度     Residual Deviance
# AIC            AIC
summary(fit) # ワルド検定を実施しているので調べる
logLik(fit)  # 最大対数尤度　板書のlogL(β0,β1)のこと

# fで説明　　 λ = log(β0 + β1f)　というモデル
fit_f <- glm(y~f, data = dat_2, family = poisson) #yをxで推定、familyで利用する分布を指定している
summary(fit_f)
logLik(fit_f)

# x,fで説明　　 λ = log(β0 + β1f + β2f)　というモデル
fit_all <- glm(y~x + f, data = dat_2, family = poisson) #yをxで推定、familyで利用する分布を指定している
summary(fit_all)
logLik(fit_all)

# AICでのモデル比較
AIC(fit)
AIC(fit_f)
AIC(fit_all)


# 尤度比検定
# anova関数にglmの戻り値を入れることで、カイ2乗分布の尤度比検定を行ってくれる
anova(fit, fit_all, test = 'Chisq')    # xのみモデルとallモデルを比較
anova(fit_f, fit_all, test = 'Chisq')  # fのみモデルとallモデルを比較
