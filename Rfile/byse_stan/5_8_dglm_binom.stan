// 二項分布を仮定したDGLM
data{
  int T;
  int len_obs;          // 観測値が得られた個数
  int y[len_obs];       // 観測値
  int obs_no[len_obs];  // 観測値が得られた時点
}

parameters{
  vector[T] mu;
  real<lower=0> s_w; // 仮定誤差の標準偏差
}

model{
  //弱情報事前分布
  s_w ~ student_t(3, 0, 10);
  // 状態方程式の実装
  for(i in 2:T){
  mu[i] ~ normal(mu[i-1], s_w);
  }
  // 観測方程式の実装
  for(i in 1:len_obs){
  y[i] ~  bernoulli_logit(mu[obs_no[i]]); //リンク関数がlogitのベルヌーイ
  }
}

generated quantities{
  vector[T] probs;        // 勝率を格納
  probs = inv_logit(mu);  // inv_logitはlogitの逆数なのでlogistic関数
}
