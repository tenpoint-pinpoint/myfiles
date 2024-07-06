data{
  int  T;
  vector[T] ex; // 説明変数
  vector[T] y;  // 観測値
}

parameters{
  vector[T] mu;  // 水準成分 -> 切片
  vector[T] b;   // 時変成分 -> 係数
  real<lower=0> s_w; //状態の標準偏差
  real<lower=0> s_t; //時変の標準偏差 -> 大きいほど時点毎の係数の値の振れ幅が大きい
  real<lower=0> s_v; //観測値の標準偏差
}

transformed parameters{
  vector[T] alpha; // 各成分の和として得られる状態推定値
  for(i in 1:T){
    alpha[i] = mu[i] + b[i] * ex[i];
  }
}

model{
  for(i in 2:T){
    mu[i] ~ normal(mu[i-1], s_w);
    b[i] ~ normal(b[i-1], s_t);
  }
  for(i in 1:T){
    y[i] ~ normal(alpha[i], s_v);
  }
}

