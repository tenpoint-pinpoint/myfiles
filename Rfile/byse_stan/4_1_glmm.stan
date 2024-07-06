data{
  int N;           // サンプルサイズ
  int fish_num[N]; // 釣果
  vector[N] sunny; // 晴れダミー
  vector[N] temp;  // 気温 
}

parameters{
  real Intercept;
  real b_temp;
  real b_sunny;
  vector[N] r;           // ランダム効果
  real<lower=0> sigma_r; // ランダム効果の標準偏差
}

transformed parameters{
  vector[N] lambda = Intercept + b_sunny * sunny + b_temp* temp + r;
}

model{
  r ~ normal(0, sigma_r);
  fish_num ~ poisson_log(lambda); // poisson_log = poisson(exp(λ))
}
