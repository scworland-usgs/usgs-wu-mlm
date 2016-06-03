data {
  int<lower=1> N; // number of observations (counties)
  int<lower=1> K; // number of predictors including column of ones
  int<lower=1> J; // number of groups (census regions)
  matrix[N,K] X; // design matrix, X[,1] is column of ones
  vector[N] y; // response variable
  int<lower=1,upper=J> census_region[N]; // groups
  real<lower = 0> nu; // LKJ parameter
}

parameters {
  matrix[K,J] z;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=1E-6>[K] tau;  // prior scale
  real<lower=1E-6> sigma; // prediction error scale 
  row_vector[K-1] b0;
  matrix[J-1,K] g0;
}

transformed parameters {
  matrix[J,K] gamma;
  matrix[J,K] beta;
  
  if (J > 1) {
    gamma[2:J] <- g0;
    for (k in 1:K)
      gamma[1,k] <-  -sum(g0[,k]);
  } else {
    gamma[1] <- rep_row_vector(0.0, K);
  }
  
  beta <- rep_matrix(append_col(rep_row_vector(0.0,1), b0), J) + gamma ;
  beta <- beta + (diag_pre_multiply(tau, L_Omega) * z)';
  #beta <- beta + (diag_matrix(tau) * z)';
}

model {
  vector[N] x_beta_census_region;
  
  tau ~ cauchy(0,1);
  sigma ~ normal(0,1);
  to_vector(z) ~ normal(0,1);
  b0 ~ cauchy(0,5);
  to_vector(g0) ~ normal(0,1);
  L_Omega ~ lkj_corr_cholesky(nu);
  
  for (n in 1:N) {
    x_beta_census_region[n] <- X[n] * beta[census_region[n]]';
  }
    y ~ normal(x_beta_census_region, sigma);
  }
    
    
    