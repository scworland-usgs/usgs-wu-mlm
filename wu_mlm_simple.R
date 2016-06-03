
library(dplyr)
library(tidyr)
library(rstan)
library(shinystan)
library(ggplot2)
library(GGally)
library(lme4)
library(arm)
library(ggmcmc)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd(getwd())

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')
wudata$wn[which(wudata$wn>1000)] = 1000 # temporary
wudata$wn[which(wudata$wn==0)] = 1 # temporary

# prepare model data
model.data <- wudata[,c(1,5,10:ncol(wudata),7)]
model.data$CDC_urban <- as.integer(model.data$CDC_urban)
n <- ncol(model.data)
model.data[,5:n] <- scale(model.data[,5:n]) # scale

# fit stan model
X <- model.matrix( ~., model.data[,5:n])

stan_data <- list(y = model.data$wn,
                  year = model.data$year,
                  X = X,
                  K = ncol(X),
                  N = nrow(model.data),
                  J = length(unique(model.data$year)))

stan_model <- stan_model('wu_mlm_regression.stan', model_name = "mlm wu regression")
stan.fit <- sampling(stan_model, stan_data, iter = 2000, chains = 4)
