# load packages
pacman::p_load(dplyr, tidyr, rstan, shinystan, ggplot2, GGally, lme4, 
               arm, ggmcmc, stringr, reshape2, magrittr, devtools, car,
               loo) 


# load data
# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/clean_scripts")
load("model.data.Rda")

# fraction of data for testing (runs faster)
set.seed(1)
model.data <- sample_frac(model.data,0.05)


# fit model in stan
nrows <- nrow(model.data)
ncols <- ncol(model.data)
X <- model.matrix( ~., model.data[,4:ncols])

# Lasso parameters to test using cv
var <- expand.grid(var1 = c(1e-5,1e-4,1e-3), 
                   var2 = c(1e-5,1e-4,1e-3,1e-2,0.1,1))

store <- var %>% mutate(elpd_loo=NA, se_elpd_loo=NA)

for(i in 1:nrow(var)) {
  cv.data <- list(y = model.data$wn,
                  X = X,
                  K = ncol(X),
                  N = nrows,
                  var1 = var$var1[i],
                  var2 = var$var2[i])
  
  
  cv.fit <- stan(file="lm_log_lik_lasso.stan", data=cv.data, iter=1000, chains=2)
  #ggs_caterpillar(ggs(cv.fit, family="beta"))
  
  log_lik <- extract_log_lik(cv.fit, parameter_name = "log_lik")
  loo <- loo(log_lik)
  
  store$elpd_loo[i] <- loo$elpd_loo
  store$se_elpd_loo[i] <- loo$se_elpd_loo
}

limits <- aes(var1, ymax = elpd_loo + se_elpd_loo, 
              ymin=elpd_loo - se_elpd_loo,
              color=factor(var2))

ggplot(store) + geom_line(aes(var1,elpd_loo, color=factor(var2))) + 
  geom_point(aes(var1,elpd_loo, color=factor(var2))) + scale_x_log10() +
geom_errorbar(limits, width=0.03) +
  labs(x=expression(theta~laplace~beta~prior), 
       color = expression(theta~Cauchy~sigma~prior),
       y="loo-expected log pointwise predictive density") +
  ggtitle("Bayesian Lasso CV shrinkage parameters")
