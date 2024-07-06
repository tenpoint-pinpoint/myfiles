// DGLM + ランダム効果モデル
// 2次のトレンド、周期性はなし
data{
  int T;         // 期間
  vector[T] ex;  // 説明変数,ここではtemperature
  int y[T];     // 観測値,ここではfish_num
}

parameters{
  vector[T] mu;       // 水準+ドリフト成分の推定値
  vector[T] r;        // ランダム効果
  real b;             // 係数
  real<lower=0> s_r;  // ランダム効果の変動の大きさ
  real<lower=0> s_z;  // ドリフト成分の変動の大きさ
}

transformed parameters{
  vector[T] lambda;
  for(i in 1:T){
    lambda[i] = mu[i] + b * ex[i] + r[i];
  }
}

model{
  // ランダム効果
  r ~ normal(0, s_r);  // ベクトル化している？
  // 水準+ドリフト成分
  for(i in 3:T){
    mu[i] ~ normal(2 * mu[i-1] - mu[i-2], s_z);
  }
  // 観測方程式
  for(i in 1:T){
    y[i] ~ poisson_log(lambda[i]);
  }
}

generated quantities{
  // 状態推定値
  vector[T] lambda_exp;
  // ランダム効果を除いた状態推定値
  vector[T] lambda_smooth;
  // ランダム効果を除き、説明変数を固定した状態推定値
  vector[T] lambda_smooth_fix;
  
  lambda_exp = exp(lambda);
  lambda_smooth = exp(mu + b * ex);
  lambda_smooth_fix = exp(mu + b * mean(ex));
}

