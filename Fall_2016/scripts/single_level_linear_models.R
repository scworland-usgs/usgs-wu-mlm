
#load libraries
library(pacman)
p_load(dplyr,ggplot2,coefplot,rstan,arm,ggmcmc,coda, rethinking)

# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/Fall_2016")

# select only numeric covariates
lm.data <-  model.data %>%
  filter(climate_region=="West")

# custom function
grab.num <- function(x){as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))}
select <- dplyr::select
rename <- dplyr::rename

# simple linear model ----
lm1 <- lm(wh~., data=select(lm.data, wh:papt))
coefplot::coefplot(lm1, intercept=F, sort="magnitude")

# Bayesian linear model fit in Stan ----
X <- model.matrix(~.,select(lm.data,cnty_pop:papt))
X <- X[,order(colnames(X))]

## list of data
stan.data1 <- list(y = lm.data$wh,
                   X = X,
                   K = ncol(X),
                   N = nrow(X))

## fit model
stan.model <- stan_model('scripts/stan/lm_normal.stan')
stan.fit1 <- sampling(stan.model, stan.data1, iter = 1000, chains = 2)

## extract samples
s <- ggs(stan.fit1,family="beta") %>%
  mutate(num = grab.num(Parameter)) %>%
  mutate(Parameter =  colnames(X)[num]) %>%
  group_by(Parameter) %>%
  summarize(low = HPDI(value,0.67)[1],
            high = HPDI(value,0.67)[2],
            lower = HPDI(value,0.89)[1],
            higher = HPDI(value,0.89)[2],
            mode = chainmode(value)) %>%
  mutate(Parameter=factor(Parameter, 
                          levels=Parameter[order(mode)], 
                          ordered=TRUE)) 

## plot parameter estimates
ggplot(filter(s, Parameter !="(Intercept)")) + theme_bw() +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange(aes(x=Parameter,y=mode,ymin=low,ymax=high), shape=21,
                  fill="white") + coord_flip() +
  geom_point(aes(Parameter,lower),shape="+", size=2) + 
  geom_point(aes(Parameter,higher),shape="+", size=2) +
  labs(y="67% and 89% HPDI with MAP point estimate") +
  ggtitle("Fully pooled parameter estimates, Laplace prior") +
  labs(subtitle="–o– = 67% HPDI,    + –o– + = 89% HPDI")

## posterior predictive densities
post.preds <- ggs(stan.fit1,family="y_pred") %>%
  mutate(num = grab.num(Parameter)) %>%
  group_by(num) %>%
  summarize(est.low = HPDI(value,0.67)[1],
            est.high = HPDI(value,0.67)[2],
            est = chainmode(value)) %>%
  mutate(obs = lm.data$wh,
         fips = lm.data$cnty_fips,
         county = lm.data$county,
         state = lm.data[,'state.abb'],
         loc = paste(county,state,sep=", "))

## plot posterior predictions
ggplot(post.preds, aes(x=obs,y=est)) +
  geom_pointrange(aes(ymin=est.low,ymax=est.high), shape=21, fill="grey", alpha=0.5) +
  #geom_text(data=filter(post.preds, abs(obs-est)>5e4),aes(label=fips),nudge_y = 15000) +
  theme_bw() + geom_abline(intercept=0, slope=1, linetype="dashed") +
  ggtitle("Posterior predictions for western climate region") +
  labs(subtitle="–o– = 67% HPDI and point is the MAP",x="observed",y="estimated") +
  coord_cartesian(xlim=c(45000,300000))




