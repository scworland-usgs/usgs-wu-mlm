

data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
}

parameters {
  vector[K] beta;
  real<lower = 0> sigma;
}

model {
  vector[N] mu;
  mu <- X * beta;
  
  //priors
  beta ~ double_exponential(0,0.001);
  sigma ~ cauchy(0,0.001);
  
  //likelihood
  y ~ normal(mu, sigma);
}

