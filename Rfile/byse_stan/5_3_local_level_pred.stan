data{
  int T;         // データ取得期間
  vector[T] y;   // 観測値
  int pred_term; // 予測期間
}

parameters{
  vector[T] mu;
  real<lower=0> s_w;  // 状態（過程誤差）の標準偏差
  real<lower=0> s_v;  // 観測（観測誤差）の標準偏差
}

model{
  // 状態方程式
  for(i in 2:T){
    mu[i] ~ normal(mu[i-1], s_w);
  }
  // 観測方程式
  for(i in 1:T){
    y[i] ~ normal(mu[i], s_v);
  }
}

// モデルとは別に事後分布を得たい
generated quantities{
  vector[T + pred_term] mu_pred; // 予測値も含めた状態の推定値
  mu_pred[1:T] = mu;             // データ取得期間は推定したmuを使う
  // データ取得期間外は予測を行う
  // 予測期間では１つ前の状態から状態を更新。パラメタを推測している
  for(i in 1:pred_term){
    mu_pred[T + i] = normal_rng(mu_pred[T + i - 1] ,s_w); //これは状態方程式を記述している
  }
}

