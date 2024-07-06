// 2次トレンドを採用した基本構造時系列モデルの実装
data{
  int T;
  vector[T] y;
}
parameters{
  vector[T] mu;
  vector[T] gamma;    // 季節成分の推定、データ点の数だけ用意
  real<lower=0> s_z;  // ドリフト成分の変動の大きさ
  real<lower=0> s_v;  // 観測誤差
  real<lower=0> s_s;  // 季節成分の変動の大きさ） 
}

transformed parameters{
  vector[T] alpha; // 状態推定値
  for(i in 1:T){
    alpha[i] = mu[i] + gamma[i];
  }
}

model{
  // 水準+ドリフト成分 -> ２次のトレンドなのでt=3から
  for(i in 3:T){
    mu[i] ~ normal(2 * mu[i-1] - mu[i-2], s_z);
  }
  //季節成分 -> 7日周期っぽいのでt=7から
  for(i in 7:T){
    gamma[i] ~ normal(-sum(gamma[(i-6):(i-1)]), s_s);
  }
  // 観測方程式
  for(i in 1:T){
    y[i] ~ normal(alpha[i], s_v);
  }
}




