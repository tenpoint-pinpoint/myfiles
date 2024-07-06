data{
  int T;       //データの取得期間
  vector[T] y; //観測値
}

parameters{
  vector[T] mu;
  real<lower=0> s_w; //過程誤差のsd
  real<lower=0> s_v; //観測誤差のsd
}

model{
  for(i in 2:T){
    mu[i] ~ normal(mu[i-1], s_w); //状態方程式を記述
  }
  for(i in 1:T){
    y[i] ~ normal(mu[i], s_v); //観測方程式を記述
  }
}

