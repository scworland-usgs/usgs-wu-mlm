data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of predictors including column of ones
  int<lower=1> J; // number of groups (years)
  matrix[N, K] X; // design matrix, X[,1] is column of ones
  vector[N] y; // response variable
  int<lower=1,upper=N> year[N]; // I think have to include this for the groups
}

parameters {
  real<lower=0> sigma; // global sigma
  real<lower=0> sigma_b; // sigma for slope parameters
  vector[J] b; // vector of estimated parameters. Should this be "matrix[J,K] b"?
  real mu_b; // mean for betas
}

transformed parameters {
  vector[N] y_hat;

  for (i in 1:N)
    y_hat[i] <- b[year[i]] * X[i]; 

}

model {
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 100);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);
  y ~ normal(y_hat, sigma);
}

}



