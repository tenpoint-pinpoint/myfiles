## 最尤法
## 尤度関数 = p^3 * (1-p)^2

# 尤度関数を作る
lik <- function(p){return(p^3 * (1-p)^2)}
# 尤度関数を描画する
curve(lik, from = 0.0, to = 1.0)

#-- ロジスティック回帰 ---------------------------------------------#
dat <- read.csv("./lecture04/keiyaku.csv", fileEncoding = "cp932")

# データの様子を俯瞰しておく
head(dat, 5)
str(dat)

# 訪問時刻と契約結果の関係
table(dat$訪問時刻, dat$契約結果)
#---結果--------------------------------------------#
#       0  1
# 午後  3  6
# 午前 11  2
#---------------------------------------------------#

# 年齢と契約結果の関係
plot(dat$担当者年齢, dat$契約結果)

#　GML：一般化線形モデル

# 分析

# glm：一般化線形モデル。ロジスティック回帰を含む広い枠組み
family = 'binomial'# ロジスティック回帰をする指示
result1 <- glm(契約結果 ~ 訪問時刻 + 担当者年齢, data = dat, family = 'binomial')
summary(result1)

#---結果--------------------------------------------#
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.0333  -0.5855  -0.1220   0.2713   1.6952  

# Coefficients:
#               Estimate   Std. Error  z value  Pr(>|z|)  
# (Intercept)    -7.3603     3.8249     -1.924   0.0543 .
# 訪問時刻午前   -3.3487     1.6552     -2.023   0.0431 *
#  担当者年齢     0.2581     0.1260      2.048   0.0406 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 28.841  on 21  degrees of freedom
# Residual deviance: 14.991  on 19  degrees of freedom
# AIC: 20.991
# Number of Fisher Scoring iterations: 6
#---------------------------------------------------#


# 分析２
result2 <- glm(契約結果 ~ 担当者年齢, data = dat, family = 'binomial')
summary(result2)

#---結果--------------------------------------------#
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.3806  -0.8724  -0.3276   0.7714   2.2721  

# Coefficients:
#                Estimate   Std. Error z value  Pr(>|z|)  
# (Intercept)     -7.25235    3.29099  -2.204   0.0275 *
#  担当者年齢      0.19791    0.09179   2.156   0.0311 *
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 28.841  on 21  degrees of freedom
# Residual deviance: 21.620  on 20  degrees of freedom
# AIC: 25.62
# Number of Fisher Scoring iterations: 5
#---------------------------------------------------#

# AICでモデルを比較
AIC(result1) #---->こちらの方が小さいので、より良いモデルである
AIC(result2)



# ロジスティック関数を作る
logistic <- function(p){return(1.0/(1.0+exp(-p)))}
curve(logistic, from = -3.0, to = 3.0)

# 編回帰係数の推定の仕組み
lik <- function(p){return(p^3 * (1-p)^2)}
curve(lik, from = 0.0, to = 1.0)

#---------------------------------------------------#


