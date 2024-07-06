data{
  int X;
  int N;
}

parameters{
  real<lower=0, upper=1> p;
}

model{
  X ~ binomial(N,p);
  p ~ beta(10,10)
}