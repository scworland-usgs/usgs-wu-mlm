library(dplyr)
library(tidyr)
library(rstan)
library(shinystan)
library(ggplot2)
library(GGally)
library(glmnet)
library(corrplot)
library(lme4)
library(lmerTest)
library(arm)
library(ggmap)
library(RColorBrewer)
library(ggmcmc)
library(stringr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')
wudata$wn[which(wudata$wn>1000)] = 1000 # temporary
wudata$wn[which(wudata$wn==0)] = 1 # temporary
n <- ncol(wudata)

# prepare model data
model.data <- wudata
model.data[,12:(n-1)] <- scale(model.data[,12:(n-1)]) # scale

cormat <- cor(model.data[,11:n])
corrplot::corrplot(cormat,method="ellipse", diag=F, type = "lower")

# multivariate linear models ----

model.data.1985 <- subset(model.data, year == 1985)
X <- model.matrix( ~., model.data.1985[,12:n])

## using lm
lm.fit <- lm(wn ~., data=model.data.1985[,11:n])

## large mlm using lmer
region = "CDC_urban"

lmer.formula <- reformulate(paste0("(1 | ", region, ") + ", 
                                   paste0(colnames(model.data[,columns])," + 
                                          (1 + ",colnames(model.data[,columns]), "|",
                                          region,")",collapse="+")), response = 'wn')

lmer.fit <- lmerTest::lmer(lmer.formula, data = model.data)

d1 <- data.frame(table(wudata$climate_region))
cf = coef(lmer.fit)$climate_region
cf$region <- rownames(cf)
cfm <- melt(cf[,2:ncol(cf)])

ggplot(cfm) + geom_point(aes(value, region), size = 2) + geom_line(aes(value, region), color="blue") +
  facet_wrap(~variable, scales = "free_x") 


# Bayesian fit ----
stan_model <- stan_model('wu_simple_reg.stan', model_name = "simple wu regression")

## 1985
model.data.1985 <- subset(model.data, year == 1985)
X1985 <- model.matrix( ~., model.data.1985[,12:n])

stan_data1985 <- list(y = model.data.1985$wn,
                  X = X1985,
                  K = ncol(X1985),
                  N = nrow(model.data.1985))

stan.fit1985 <- sampling(stan_model, stan_data1985, iter = 2000, chains = 4)
fit_ggs1985 <- ggs(stan.fit1985, family="beta")
fit_ggs1985$num <- as.numeric(gsub("[^\\d]+", "", fit_ggs1985$Parameter, perl=TRUE)) # extract beta #'s
fit_ggs1985$Parameter <- colnames(X1985)[fit_ggs1985$num] # index parameters


## 1990
model.data.1990 <- subset(model.data, year == 1990)
X1990 <- model.matrix( ~., model.data.1990[,12:n])

stan_data1990 <- list(y = model.data.1990$wn,
                      X = X1990,
                      K = ncol(X1990),
                      N = nrow(model.data.1990))

stan.fit1990 <- sampling(stan_model, stan_data1990, iter = 2000, chains = 4)
fit_ggs1990 <- ggs(stan.fit1990, family="beta")
fit_ggs1990$num <- as.numeric(gsub("[^\\d]+", "", fit_ggs1990$Parameter, perl=TRUE)) # extract beta #'s
fit_ggs1990$Parameter <- colnames(X1990)[fit_ggs1990$num] # index parameters

## 1995
model.data.1995 <- subset(model.data, year == 1995)
X1995 <- model.matrix( ~., model.data.1995[,12:n])

stan_data1995 <- list(y = model.data.1995$wn,
                      X = X1995,
                      K = ncol(X1995),
                      N = nrow(model.data.1995))

stan.fit1995 <- sampling(stan_model, stan_data1995, iter = 2000, chains = 4)

## 2000
model.data.2000 <- subset(model.data, year == 2000)
X2000 <- model.matrix( ~., model.data.2000[,12:n])

stan_data2000 <- list(y = model.data.2000$wn,
                      X = X1995,
                      K = ncol(X1995),
                      N = nrow(model.data.2000))

stan.fit2000 <- sampling(stan_model, stan_data2000, iter = 2000, chains = 4)

## 2005
model.data.2005 <- subset(model.data, year == 2005)
X2005 <- model.matrix( ~., model.data.2005[,12:n])

stan_data2005 <- list(y = model.data.2005$wn,
                      X = X2005,
                      K = ncol(X2005),
                      N = nrow(model.data.2005))

stan.fit2005 <- sampling(stan_model, stan_data2005, iter = 2000, chains = 4)

## 2010
model.data.2010 <- subset(model.data, year == 2010)
X2010 <- model.matrix( ~., model.data.2010[,12:n])

stan_data2010 <- list(y = model.data.2010$wn,
                      X = X2010,
                      K = ncol(X2010),
                      N = nrow(model.data.2010))

stan.fit2010 <- sampling(stan_model, stan_data2010, iter = 2000, chains = 4)



ggs_caterpillar(list(y1985=fit_ggs1985, y1990=fit_ggs1990)) + geom_vline(xintercept = 0) + 
  theme_bw(base_size=12) + xlab('HDI')

















# plot coefficients w/ lm estimates
lm.coef <- data.frame(coef(summary(lm.fit))[,1:2] )
lm.coef$Parameter <- rownames(lm.coef)
rownames(lm.coef) <- NULL
colnames(lm.coef)[1:2] <- c("value.lm","error.lm")

fit_ggs <- ggs(stan.fit1, family="beta")
fit_ggs$num <- as.numeric(gsub("[^\\d]+", "", fit_ggs$Parameter, perl=TRUE)) # extract beta #'s
fit_ggs$Parameter <- colnames(X)[fit_ggs$num] # index parameters
fit_ggs2 <- merge(fit_ggs, lm.coef, by ="Parameter")

ggs_caterpillar(fit_ggs2) + geom_vline(xintercept = 0) + theme_bw(base_size=12) + xlab('Parameter Values') +
  ggtitle("Parameter Estimates for 2005 wn") + 
  geom_point(aes(x=value.lm, y=Parameter), color="dodgerblue", size=3, alpha=0.5) +
  geom_segment(aes(x = value.lm-error.lm, y = Parameter, xend = value.lm+error.lm, yend = Parameter), 
               color="dodgerblue", alpha=0.8) 


## histogram with coeff
ggplot(data=fit_ggs1985) + geom_histogram(aes(value),bins=75) + 
  geom_vline(data=lm.coef,aes(xintercept=value.lm, 
                              colour = value.lm),
             size = 1,
             show.legend = TRUE) +
  facet_wrap(~Parameter, ncol = 1) + theme_bw(base_size=4)

