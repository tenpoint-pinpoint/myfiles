data{
  int T;
  vector[T] y;
}

parameters{
  vector[T] mu;
  vector[T] delta;
  real<lower=0> s_w; // 過程誤差
  real<lower=0> s_v; // 観測誤差
  real<lower=0> s_z; // ドリフト成分の変動の大きさ
}

model{
  // 弱情報事前分布
  s_w ~ normal(2, 2);
  s_v ~ normal(10, 5);
  s_z ~ normal(0.5, 0.5);
  // 状態方程式の実装
  for(i in 2:T){
    delta[i] ~ normal(delta[i-1], s_z);
    mu[i] ~ normal(mu[i-1] + delta[i-1], s_w);
  }
  // 観測方程式の実装
  for(i in 1:T){
    y[i] ~ normal(mu[i], s_v);
  }
}
