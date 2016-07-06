data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
}

parameters {
  vector[K] beta;
  vector<lower=0>[K] lambda;
  real<lower=0> tau;
  real<lower=0> sigma;
}

model {
  lambda ~ cauchy(0, 0.00001);
  tau ~ cauchy(0, 0.00001);
  
  for (i in 1:K)
    beta[i] ~ normal(0, lambda[i] * tau);
    sigma ~ cauchy(0,0.001);
  y ~ normal(X * beta, sigma);
}
