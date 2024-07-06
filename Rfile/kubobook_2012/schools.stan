data {
  int<lower=0> J; // �w�Z�̐�
  real y[J]; // ���肳��Ă��鋳��̌���
  real<lower=0> sigma[J]; // ����̌��ʂ̕W���덷
}

parameters {
  real mu; // ���u�̌���(�S�̕���)
  real<lower=0> tau; // ���u�̌��ʂ̕W���΍�
  vector[J] eta; // �w�Z���Ƃ̃X�P�[���O�̃o����
}

transformed parameters {
  vector[J] theta = mu + tau * eta; // �w�Z���Ƃ̏��u�̌���
}

model {
  target += normal_lpdf(eta | 0, 1); // ���O���z�̑ΐ����x
  target += normal_lpdf(y | theta, sigma); // �ΐ��ޓx
}

