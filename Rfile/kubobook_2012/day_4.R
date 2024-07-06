library(rstan)
library(dplyr)

d <- read.csv("./binomial/data4a.csv")

d$f <- as.factor(d$f)
head(d)

# glm推定
fit.glm <- glm(cbind(y,N-y)?x+f, data = d, family = binomial)

# ベイズで推定

# stanはダミー変数化してくれないので前処理する
dat <- d %>% mutate(fT = case_when(f=="T" ?1 ,TRUE ?0)) %>% 
  select(-f)

# stanに渡すリスト作成
seed.dat <- list(
  y=dat$y,
  x=dat$x,
  N=dat$N,
  fT=dat$fT,
  n=length(dat$y)
)

# stanモデルを作る
logistic.model <- '
data {
int n; // サンプルサイズ
int<lower=0> N[n]; // 観測種子数
int<lower=0> y[n]; // 生存種子数
real<lower=0> x[n]; //体サイズ
int<lower=0,upper=1> fT[n]; //ダミー変数化した施肥処理
}
parameters {
real beta0; //切片
real betax; //xの係数
real betaf; //fの係数
}
transformed parameters {
real<lower=0,upper=1> q[n]; //生存確率
for(i in 1:n){
q[i] = inv_logit(beta0+betax*x[i]+betaf*fT[i]); //生存確率の回帰式
}
}
model {
for(i in 1:n){
y[i] ~ binomial(N[i],q[i]); //二項分布で生存種子数をモデリング
}
beta0 ~ normal(0,100); //回帰係数の無情報事前分布
betax ~ normal(0,100); //平均0,標準偏差100の正規分布
betaf ~ normal(0,100);
}
'
logistic.model

# 
set.seed(1)

# モデルへの当てはめ
fit <- stan(
  model_code = logistic.model,
  data = seed.dat,
  iter = 1000, #MCMCステップ数
  warmup=100, #最初のwarmupステップは無視する(p.181参照)
  thin = 1, #間引き感覚
  chains = 3 #並列繰り返し
)
#中身を見る
fit

# 各パラメータだけみる
print(fit, pars = "q")
print(fit, pars =c("beta0","betax","betaf"))

#Rhatだけ確認するなら下記も使えます.
stan_rhat(fit) #rhatのヒストグラム.
all(stan_rhat(fit)$data < 1.10, na.rm = T) #全部1.10未満ならTRUEを返す.

#パラメータごとにtraceplotも確認.
traceplot(fit, pars ="beta0")
traceplot(fit, pars ="betax")
traceplot(fit, pars ="betaf")

#問題がなさそうなので, 最後にglm()で得られた最尤推定値と比較します. 
beta0 <- get_posterior_mean(fit, par = 'beta0')[, 'mean-all chains']# 平均が入っている場所
betax <- get_posterior_mean(fit, par = 'betax')[, 'mean-all chains']
betaf <- get_posterior_mean(fit, par = 'betaf')[, 'mean-all chains']
print(c(beta0,betax,betaf))
#glm()による最尤推定値
fit.glm$coefficient