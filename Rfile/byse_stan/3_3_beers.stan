data{
  int N;
  vector[N] sales;
  vector[N] temperature;

  int N_pred; //　追加データ：予測データの大きさ
  vector[N_pred] temperature_pred; //　追加データ：予測のための気温データ
}

parameters{
  real Intercept;
  real beta;
  real<lower=0> sigma;
}

# 求めたいパラメータから生成されるデータの生成規則。ここではy=ax+bとなり、yの期待値がmuとなる
# modelで得られるものがパラメータの事後分布、パラメータの事後分布からパラメータの推定を行い、モデル推定を行う
model{
  for (i in 1:N){
    // 平均がIntercept *  beta * temperature、分散がsigmaの正規分布に従う
    sales[i] ~ normal(Intercept + beta * temperature[i], sigma);
  }
}

generated quantities{ // モデル推定以外の事後分布を得たい場合に使う。今回は予測分布を作りたい
  vector[N_pred] mu_pred;     // ビールの売上期待値
  vector[N_pred] sales_pred; // ビールの売上予測

  for (i in 1:N_pred){
    mu_pred[i] = Intercept + beta * temperature_pred[i];
    sales_pred[i] = normal_rng(mu_pred[i], sigma);
  }
}
