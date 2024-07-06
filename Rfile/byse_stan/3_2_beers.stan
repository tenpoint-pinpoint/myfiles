data{
  int N;
  vector[N] sales;
  vector[N] temperature;
}

parameters{
  real Intercept;
  real beta;
  real<lower=0> sigma;
}

model{
  for (i in 1:N){
    // 平均がIntercept *  beta * temperature、分散がsigmaの正規分布に従う
    sales[i] ~ normal(Intercept + beta * temperature[i], sigma);
  }
}



