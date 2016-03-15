data {
  int<lower=1> N;
  int<lower=1> K;
  int<lower=1> J;
  matrix[N, K] X;
  vector[N] y;
  int<lower=1,upper=N> year[N];
}

parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  vector[J] b;
  real mu_a;
  real mu_b;
}

transformed parameters {
  vector[N] y_hat;

  for (i in 1:N)
    y_hat[i] <- a[year[i]] + b[year[i]] * X[i];

}

model {
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 100);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);
  y ~ normal(y_hat, sigma);
}

}



