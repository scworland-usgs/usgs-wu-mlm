
# load packages
pacman::p_load(dplyr, tidyr, rstan, shinystan, ggplot2, GGally, lme4, 
               arm, ggmcmc, stringr, reshape2, magrittr, devtools, car,
               loo) 


# load data
# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/clean_scripts")
load("model.data.Rda")
nrows <- nrow(model.data)

# fraction of data for testing (runs faster)
set.seed(1)
model.data <- sample_frac(model.data,0.05)

# cv metrics for models
names = c("null","climate","climate_landcover","all","all_shrinkage")
models <- data.frame(names=names,
                     elpd_loo=rep(NA,5),
                     p_loo=rep(NA,5),
                     looic=rep(NA,5))

# fit no predictor model in stan ----
cv.fit <- stan(file="log_lik_nopreds.stan", 
               data=list(y=model.data$wn, N=nrow(model.data)), 
               iter=500, chains=2)

log_lik <- extract_log_lik(cv.fit, parameter_name = "log_lik")
loo <- loo(log_lik)

## add to master
models[1,2:4] = loo[1:3]

# fit climate only model in stan ----
X1 <- model.matrix( ~., model.data[,c(4,5,7)])

cv.data1 <- list(y = model.data$wn,
                 X = X1,
                 K = ncol(X1),
                 N = nrows)

cv.fit1 <- stan(file="lm_log_lik.stan", data=cv.data1, iter=500, chains=2)
#ggs_caterpillar(ggs(cv.fit, family="beta"))

log_lik1 <- extract_log_lik(cv.fit1, parameter_name = "log_lik")
loo1 <- loo(log_lik1)

## add to master
models[2,2:4] = loo1[1:3]

# fit climate + landcover model in stan ----
X2 <- model.matrix( ~., model.data[,c(4,5,7,14:16)])

cv.data2 <- list(y = model.data$wn,
                 X = X2,
                 K = ncol(X2),
                 N = nrows)

cv.fit2 <- stan(file="lm_log_lik.stan", data=cv.data2, iter=500, chains=2)

log_lik2 <- extract_log_lik(cv.fit2, parameter_name = "log_lik")
loo2 <- loo(log_lik2)

## add to master
models[3,2:4] = loo2[1:3]

# fit all in stan ----
X3 <- model.matrix( ~., model.data[,4:ncol(model.data)])

cv.data3 <- list(y = model.data$wn,
                 X = X3,
                 K = ncol(X3),
                 N = nrows)

cv.fit3 <- stan(file="lm_log_lik.stan", data=cv.data3, iter=500, chains=2)

log_lik3 <- extract_log_lik(cv.fit3, parameter_name = "log_lik")
loo3 <- loo(log_lik3)

## add to master
models[4,2:4] = loo3[1:3]

# fit all with lasso in stan ----
cv.fit4 <- stan(file="lm_log_lik_lasso.stan", data=cv.data3, iter=500, chains=2)

log_lik4 <- extract_log_lik(cv.fit4, parameter_name = "log_lik")
loo4 <- loo(log_lik4)

## add to master
models[5,2:4] = loo4[1:3]

ggs1 <- ggs(cv.fit3, family="beta")
ggs2 <- ggs(cv.fit4, family="beta")
ggs_caterpillar(ggs1) + xlim(-0.1,0.05) + geom_vline(xintercept=0)
ggs_caterpillar(ggs2) + xlim(-0.005,0.005) + geom_vline(xintercept=0)










