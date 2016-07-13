
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
  mu = X*beta;
  
  y ~ normal(mu, sigma); // data model

}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] <- normal_log(y[n], X[n]*beta, sigma);
  }
}

