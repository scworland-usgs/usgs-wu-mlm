
data {
  int<lower=0> N; 
  vector[N] y;
}
parameters {
  vector[1] beta;
  real<lower=0> sigma;
} 
model {
  y ~ normal(beta[1],sigma);
}
  
generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] <- normal_log(y[n], beta[1], sigma);
  }
}
  