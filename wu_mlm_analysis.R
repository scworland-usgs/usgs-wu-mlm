library(dplyr)
library(tidyr)
library(rstan)
library(shinystan)
library(ggplot2)
library(GGally)
library(glmnet)
library(corrplot)
library(lme4)
library(arm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")

# prepare data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')
wudata$wn[which(wudata$wn>1000)] = 1000
wudata$wn[which(wudata$wn==0)] = 1

# scale data
sdata <- data.frame(cbind(wudata[,9], scale(wudata[,c(11:ncol(wudata)-1)])))
colnames(sdata)[1] <- "wn"

cormat <- cor(sdata)
corrplot(cormat,method="ellipse", diag=F, type = "lower")

# linear models ----
## using lm
lm.fit <- lm(wn ~., data=sdata)

## mlm using lmer
sdata$year <- wudata$year
sdata$state <- wudata$State
sdata$census_region <- wudata$census_region

lmer.formula <- reformulate(paste0("(1 | ",colnames(sdata[,2:ncol(sdata)]),")",collapse="+"),response = 'someDV')

lmer.fit <- lmer(wn ~ mhi + (mhi|state) + pvi + (pvi|state), data=sdata)

cf = coef(lmer.fit)$state
se = se.coef(lmer.fit)$state
x = data.frame(cbind(cf[,2],se[,2]))
colnames(x) = c("MHI","se")
x$state = rownames(x)
rownames(x) = NULL

p = ggplot(data=x) 
p = p + geom_point(aes(x=state, y=MHI, fill=MHI), size = 2, color = "black", shape = 21)
p = p + geom_errorbar(aes(x=state, y=MHI, ymin=MHI-se, ymax=MHI+se), width=.3, color ="black")
p = p + theme_bw(base_size=14) + coord_flip()
p

p = p + geom_point(aes(x=State, y=alpha), size = 3) + xlab("")
p = p + geom_errorbar(aes(x=State, y=alpha, ymin=alpha-se, ymax=alpha+se), 
                      width=.1, alpha=1) 
p = p + geom_hline(aes(yintercept=av), linetype=2)
p = p + theme_bw(base_size=20) + coord_flip()
p = p + ggtitle("Null model for 2010 Wn")
p = p + ylab("alpha (gal/p/day)")
p



## regularized linear model


## ridge regression
set.seed(10)
y <- as.matrix(sdata[,7]-mean(sdata[,7])) # mean centered
x <- as.matrix(sdata[,8:15])
glm1 <- cv.glmnet(x, y, alpha = 0, nfolds = 10)
plot(glm1) 

# Bayesian fit ----
stan_data <- list(vwci = sdata$vwci,
                  pvi = sdata$pvi,
                  pop = sdata$log.pop,
                  pop_growth = sdata$pop.growth,
                  rpp = sdata$rpp,
                  drought = sdata$drought,
                  temp = sdata$temp,
                  precip = sdata$precip,
                  surface_water = sdata$surface.water,
                  N = nrow(sdata))

stan_model <- stan_model('jg_analysis.stan', model_name = "Multiple Regression CA")

stan.fit <- sampling(stan_model, stan_data, iter = 2000, chains = 4)
print(stan.fit)

# plot coefficients
fit_ggs <- ggs(stan.fit, family="m")
fit_ggs <- subset(fit_ggs, Parameter != "sigma")
ggs_caterpillar(fit_ggs) + geom_vline(xintercept = 0) + theme_bw(base_size=12) + xlab('HDI') +
  ggtitle("CA as binary predictor")