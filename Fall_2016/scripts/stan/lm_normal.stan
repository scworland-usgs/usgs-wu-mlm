
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
}

parameters {
  vector[K] beta;
  real<lower = 0 > sigma;
}

model {
  vector[N] mu;
  mu = X * beta;
  
  //priors
  beta ~ normal(0,1e5);
  sigma ~ cauchy(0,1e4);
  
  //likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  real y_pred[N];
  
    for (i in 1:N) {
      y_pred[i] = normal_rng(X[i] * beta, sigma);
    }
}