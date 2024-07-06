data{
  int N;          // サンプルサイズ
  int K;          // 説明変数の数+1(=デザイン行列の列数)
  vector[N] Y;    // 目的変数
  matrix[N,K] X;  // デザイン行列
}

parameters{
  vector[K] b;          // 切片を含む係数ベクトル
  real<lower=0> sigma;  // 標準偏差
}

model{
  vector[N] mu = X * b;
  Y ~ normal(mu, sigma);
}
