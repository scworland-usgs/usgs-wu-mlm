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
library(reshape2)

setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')
wudata$wn[which(wudata$wn>1000)] = 1000 # temporary
wudata$wn[which(wudata$wn==0)] = 1 # temporary

# prepare model data
model.data <- wudata[,c(1,5,10:ncol(wudata),7)]
model.data$CDC_urban <- as.integer(model.data$CDC_urban)
model.data$year <- as.integer(factor(wudata$year))
model.data$climate_region <- as.integer(factor(model.data$climate_region))

n <- ncol(model.data)
model.data[,5:n] <- scale(model.data[,5:n]) # scale

cormat <- cor(model.data[,4:n])
corrplot::corrplot(cormat,method="ellipse", diag=F, type = "lower")

# lm and lmer models ----

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

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Years as levels ----

X <- model.matrix( ~., model.data[,5:n])

stan_data <- list(y = model.data$wn,
                  year = model.data$year,
                  X = X,
                  K = ncol(X),
                  N = nrow(model.data),
                  J = length(unique(model.data$year)),
                  nu = 2)

stan_model <- stan_model('wu_mlm_regression.stan', model_name = "mlm wu regression")

stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')
fit <- sampling(stan_model, stan_data, pars = stan_params,iter = 2000, chains = 1)

g <- ggs(fit,family="beta")
g$num <- as.numeric(gsub("[^\\d]+", "", g$Parameter, perl=TRUE)) # extract beta #'s
g$year_index <- as.numeric(substr(as.character(g$num),1,1))
g$coef_index <- as.numeric(substring(as.character(g$num), 2))
g$year <- unique(wudata$year)[g$year_index]
g$Parameter <- colnames(X)[g$coef_index]

g2 <- dcast(aggregate(value ~ Parameter + year, data=g, FUN="mean"), Parameter~year)

ggplot(g[,c(3,4,8)]) + 
  geom_boxplot(aes(x=Parameter,y=value,fill=factor(year)), size=0.5, outlier.shape=NA) + 
  geom_hline(yintercept = 0) + ggtitle("Posterior estimates for years 1985-2010") +
  coord_flip() + theme_bw(base_size=15) + labs(x=NULL, fill="year")


# Regions as levels ----
stan_data.regions <- list(y = model.data$wn,
                          climate_region = model.data$climate_region,
                          X = X,
                          K = ncol(X),
                          N = nrow(model.data),
                          J = length(unique(model.data$climate_region)),
                          nu = 2)

stan_model <- stan_model('wu_mlm_climate_region.stan', model_name = "mlm wu climate region")

stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')
fit.regions <- sampling(stan_model, stan_data.regions, pars = stan_params, iter = 2000, chains = 3)


g.regions <- ggs(fit.regions,family="beta")
g.regions$num <- as.numeric(gsub("[^\\d]+", "", g.regions$Parameter, perl=TRUE)) # extract beta #'s
g.regions$region_index <- as.numeric(substr(as.character(g.regions$num),1,1))
g.regions$coef_index <- as.numeric(substring(as.character(g.regions$num), 2))
g.regions$climate_region <- unique(wudata$climate_region)[g.regions$region_index]
g.regions$Parameter <- colnames(X)[g.regions$coef_index]


ggplot(g.regions[,c(3,4,8)]) + ylim(c(-50,50)) +
  geom_boxplot(aes(x=Parameter,y=value), fill="grey50", outlier.shape=NA) + 
  geom_hline(yintercept = 0) + ggtitle("Posterior estimates for climate regions") +
  coord_flip() + theme_bw(base_size=15) + labs(x=NULL, fill="year") + facet_wrap(~climate_region)













# plot coefficients w/ lm estimates
lm.coef <- data.frame(coef(summary(lm.fit))[,1:2] )
lm.coef$Parameter <- rownames(lm.coef)
rownames(lm.coef) <- NULL
colnames(lm.coef)[1:2] <- c("value.lm","error.lm")

fit_ggs <- ggs(stan.fit1, family="beta")
fit_ggs$num <- as.numeric(gsub("[^\\d]+", "", fit_ggs$Parameter, perl=TRUE)) # extract beta #'s
fit_ggs$Parameter <- colnames(X)[fit_ggs$num] # index parameters
fit_ggs2 <- merge(fit_ggs, lm.coef, by ="Parameter")

ggs_caterpillar(fit_ggs1985) + geom_vline(xintercept = 0) + theme_bw(base_size=12) + xlab('Parameter Values') + xlim(-50,50)
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

