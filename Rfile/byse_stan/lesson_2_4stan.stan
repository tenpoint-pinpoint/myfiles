data{
  int N;  // サンプルサイズ
  vector[N] sales;  // データ->N個あることを明示
}

parameters{
  real mu; //期待値を実数値で宣言
  real<lower=0> sigma; //標準偏差を実数値で宣言
}

model{  // 1~Nの値は平均mu、標準偏差sigmaの正規分布から得られたとする
  for (i in 1:N){
    sales[i] ~ normal(mu, sigma);
  }
}
