library(rstan)
library(dplyr)

d <- read.csv("./hbm/data7a.csv")

head(d)
hist(d$y)

# 階層ベイズモデルの作成 -> 階層事前分布でも直感的に記述できる
hie_model <- '
data {
  int<lower=0> n; #サンプルサイズ
  int<lower=0> y[n]; #種子8個当たりの生存数
}
parameters {
  real beta; #全個体共通のロジスティック偏回帰係数 
  real r[n]; #個体差
  real<lower=0> s; #個体差の標準偏差
}
transformed parameters {
  real q[n];
  for (i in 1:n){
    q[i] = inv_logit(beta+r[i]); #生存確率を個体差でロジット変換
  }
}
model {
#生存確率q[i]の二項分布
  for (i in 1:n){
    y[i] ? binomial(8,q[i]);
  }
#betaの無情報事前分布
  beta?normal(0,100);
#個体差の階層事前分布
  for (i in 1:n)
    r[i]?normal(0,s); #個体差の階層事前分布
  s?uniform(0,10000); #sの無情報事前分布
}
'


# データを作成
hie_data <- list(y = d$y, n = length(d$y))

set.seed(1)
#MCMCサンプリングによる事後分布の推定 
fit <- stan(
model_code = hie_model,
data = hie_data,
iter = 1000, #MCMCステップ数
warmup=100, #最初のwarmupステップは無視する(p.181参照) 
thin = 1, #間引き感覚
chains = 3 #並列繰り返し 
)
#各パラメータの様子
print(fit, pars ="q") print(fit, pars ="r") print(fit, pars =c("beta","s"))
stan_rhat(fit) #rhatのヒストグラム.
all(stan_rhat(fit)$data < 1.10, na.rm = T) #全部1.10未満ならTRUEを返す.
#表示しきれない場合 #options(max.print = 10000)

