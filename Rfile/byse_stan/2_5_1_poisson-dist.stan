data{
  int N;
  int animal_num[N];
}

parameters{
  real<lower=0> lambda;
}

model{
  animal_num ~ poisson(lambda);
}

generated quantities{//事後分布を得る
  int pred[N];
  for (i in 1:N){
    pred[i] = poisson_rng(lambda); // ポアソン分布の乱数
  }
}
