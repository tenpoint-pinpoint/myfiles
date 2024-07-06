data{
  int T;       //データの取得期間
  vector[T] y; //観測値
}

parameters{
  vector[T] mu;
  real<lower=0> s_z; // ドリフト成分の変動幅を表すsd
  real<lower=0> s_v; // 観測誤差のsd
}

model{
  for(i in 3:T){
    mu[i] ~ normal(2*mu[i-1] - mu[i-2], s_z); //状態方程式を記述
  }
  for(i in 1:T){
    y[i] ~ normal(mu[i], s_v); //観測方程式を記述
  }
}

