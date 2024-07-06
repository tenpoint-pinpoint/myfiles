library(rstan)
load("Y.RData")

#stanに渡すデータの作成
spatial_data <- list(y = Y, n =length(Y))

spatial_model <- ' data {
int n; // サンプルサイズ
int<lower=0> y[n]; // 個体数
}

parameters {
real beta; // 大域的な切片(全場所で共通)
real<lower=0> s; // 場所差の標準偏差を決める超パラメータ
real r0[n-1]; // 補助パラメータ
}

transformed parameters {
real r[n]; // 場所差
real mu[n]; // 場所差の期待値 
real<lower=0> std[n]; // 場所差の標準偏差

//切片が一意的に定まるように場所差の和は0とする
for(i in 1:n-1){
r[i] = r0[i]; }
r[n] = -sum(r0);

// 空間相関を指定 
mu[1] = r[2]; std[1] = s;
for (i in 2:(n-1)){
mu[i] = (r[i-1]+r[i+1])/2;
std[i] = s/sqrt(2); }
mu[n] = r[n-1];
std[n] = s; }

model {
// 無情報事前分布
beta ? normal(0,100); s ? uniform(0,10000);
for(i in 1:n){
r[i] ? normal(mu[i],std[i]); y[i] ? poisson_log(beta + r[i]);
}
}
'

#乱数のシード.説明用の設定なので実際のモデリングでは設定しない. 
set.seed(1)
#MCMCサンプリングによる事後分布の推定 
fit <- stan(
model_code = spatial_model,
data = spatial_data,
iter = 5000, #MCMCステップ数
warmup=500, #最初のwarmupステップは無視する(p.181参照) 
thin = 1, #間引き感覚
chains = 4 #並列繰り返し
)



