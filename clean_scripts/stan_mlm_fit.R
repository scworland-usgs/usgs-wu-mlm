
# load packages
library(pacman)
pacman::p_load(dplyr, tidyr, rstan, shinystan, ggplot2, GGally, lme4, 
               arm, ggmcmc, stringr, reshape2, magrittr, devtools, car) 

# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/clean_scripts")
 
# load data and custom functions
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')
load("model.data.Rda")
source_url("stan_regression_function.R")

short.names <- data.frame(variable = names(model.data[,4:17]), 
                          short.names = c("mean-ppt","max-temp","min-pdsi",
                                          "cross-pdsi","agriculture",
                                          "developed","semi-dev",
                                          "pvi","voting-pop","mhi",
                                          "poverty","college",
                                          "popGrowth","pop50"))

# set "select" function to dplyr's default
select = dplyr::select

# fraction of data for testing (runs faster)
# set.seed(1)
# model.data <- sample_frac(model.data,0.05)

# completely pooled regression model----
nrows <- nrow(model.data)
ncols <- ncol(model.data)
X <- model.matrix( ~., model.data[,4:ncols])

stan_data.pooled <- list(y = model.data$wn,
                         X = X,
                         K = ncol(X),
                         N = nrows)

## fit the pooled model... takes about 4 minutes
stan.fit.pooled <- stan.regression(wudata, 'simple_reg_stan.stan', stan_data.pooled, 
                                   stan_params=NULL, iter = 2000, chains = 4, group.name=NULL)

## extract stan object
fit.pooled_stan <- sflist2stanfit(stan.fit.pooled[1])

## extract data frame with all samples
pooled.all <- stan.fit.pooled[2] %>% data.frame()

## extract summary data frame
pooled.summary <- stan.fit.pooled[3] %>% data.frame() %>% 
  filter(., !grepl("(Intercept)", variable)) %>% 
  mutate(names="pooled") %>% 
  select(variable, names, p97.5, p2.5, p50) %>%
  inner_join(short.names, by ="variable") %>%
  mutate(short.names=factor(short.names, 
                            levels=short.names[order(p50)], 
                            ordered=TRUE))
  

## plot coefficients
ggplot(pooled.summary) + geom_hline(yintercept=0, color="black") +
  geom_linerange(aes(short.names, ymin=p2.5, ymax=p97.5), color = "dodgerblue", size=0.5) +
  geom_point(aes(short.names, p50), size=2, color="dodgerblue") + theme_bw() + xlab("") +
  #geom_point(data=lm.pooled, aes(short.names, value.lm), size=1, color="red") +
  coord_flip() + ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) +
  ggtitle("Pooled Parameter Estimates")


# stan mlm regression model for urban class ----
group.name = "CDC_urban" 
group <- model.data[,match(group.name,names(model.data))] 
nrows <- nrow(model.data)
ncols <- ncol(model.data)
X <- model.matrix( ~., model.data[,4:ncols])

## build list of data to pass to stan
stan_data.CDC_urban <- list(y = model.data$wn,
                            group = group,
                            X = X,
                            K = ncol(X),
                            N = nrows,
                            J = length(unique(group)),
                            nu = 2)

## list parameters to return (optional)
stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')

## Fit model... takes 5+ hours to run with all the data
stan.fit.urban <- stan.regression(wudata, 'mlm_stan.stan', stan_data.CDC_urban, 
                                  stan_params=stan_params, iter = 1000, chains = 2, 
                                  group.name=group.name)

## extract stan object
fit.urban_stan <- sflist2stanfit(stan.fit.urban[1])

## extract data frame with all samples
urban.all <- stan.fit.urban[2] %>% data.frame()

## extract summary data frame
rename_urban <- '"non_core"="micropolitan";"micropolitan"="small_metro";"small_metro"="non_core"'

urban.summary <- stan.fit.urban[3] %>% data.frame() %>% 
  filter(., !grepl("(Intercept)", variable)) %>%
  mutate(names = car::recode(names, rename_urban)) %>% 
  inner_join(short.names, by ="variable") 

urban.summary$names <- factor(urban.summary$names, 
                              levels = levels(wudata$CDC_urban))

cols <- c('#d53e4f','#fc8d59','#fee08b','#e6f598','#99d594','#3288bd')

# old plot
ggplot(urban.summary) + 
  geom_linerange(aes(names, ymin=p2.5, ymax=p97.5)) + 
  geom_point(aes(names, p50, fill=names), shape=21) +
  geom_hline(yintercept=0, color="black") + theme_bw(base_size=16) + 
  facet_wrap(~short.names, ncol=2) + coord_flip() + ylim(c(-0.5,0.5)) +
  xlab(expression(urban %->% rural)) + #scale_fill_brewer(palette="Spectral", guide=F) +
  scale_fill_manual(values=cols, guide=F, name = "Urban class") +
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) +
  ggtitle("Hierarchical Model Estimates for Urban Class") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


# new plot
ggplot(urban.summary) + 
  geom_pointrange(aes(names,p50, ymin=p2.5, ymax=p97.5, fill=names),  
                  shape=21, position=position_dodge(width=0.6)) + 
  scale_fill_manual(values=cols) + theme_bw() + facet_wrap(~short.names, ncol=14) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.border = element_rect(colour = "grey20", fill=NA, size=1)) +
  geom_hline(yintercept=0, color="black") +
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) + xlab(NULL)

# stan mlm regression model for census region ----
group.name = "census_region" 
group <- model.data[,match(group.name,names(model.data))] 
nrows <- nrow(model.data)
ncols <- ncol(model.data)
X <- model.matrix( ~., model.data[,4:ncols])

## build list of data to pass to stan
stan_data.census_region <- list(y = model.data$wn,
                            group = group,
                            X = X,
                            K = ncol(X),
                            N = nrows,
                            J = length(unique(group)),
                            nu = 2)

## list parameters to return (optional)
stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')

## Fit model... takes 5+ hours to run with all the data
stan.fit.census <- stan.regression(wudata, 'mlm_stan.stan', stan_data.census_region, 
                                  stan_params=stan_params, iter = 1000, chains = 2, 
                                  group.name="census_region")

# extract stan object
fit.census_stan <- sflist2stanfit(stan.fit.census[1])

# extract data frame with all samples
census.all <- stan.fit.census[2] %>% data.frame()

# extract summary data frame
rename_census <- '"West"="Northeast";"Northeast"="Midwest";"Midwest"="West"'

census.summary <- stan.fit.census[3] %>% data.frame() %>% 
  filter(., !grepl("(Intercept)", variable)) %>%
  mutate(names = car::recode(names, rename_census)) %>% 
  inner_join(short.names, by ="variable") 

census.summary$names <- factor(census.summary$names, levels = levels(wudata$census_region))

cols2 <- c('#ffffcc','#a1dab4','black','#41b6c4','#225ea8')

# old plot
ggplot(census.summary) + 
  geom_linerange(aes(names, ymin=p2.5, ymax=p97.5)) + 
  geom_point(aes(names, p50, fill=names), shape=21) +
  geom_hline(yintercept=0, color="black") + theme_bw(base_size=16) + 
  facet_wrap(~short.names, ncol=2) + coord_flip() + 
  scale_fill_manual(values=cols2) + ylim(c(-0.25,0.25)) +
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) +
  ggtitle("Hierarchical Model Estimates for Census Regions") +
  xlab(expression(West %->% East)) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# new plot 
ggplot(census.summary) + 
  geom_pointrange(aes(names,p50, ymin=p2.5, ymax=p97.5, fill=names),  
                  shape=21, position=position_dodge(width=0.6)) + 
  scale_fill_manual(values=cols2) + theme_bw() + facet_wrap(~short.names, ncol=14) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.border = element_rect(colour = "grey20", fill=NA, size=1)) +
  geom_hline(yintercept=0, color="black") +
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) + xlab(NULL)

# pairs plot
shorter.names <- data.frame(variable = names(model.data[,4:17]), 
                          short.names = c("ppt","temp","mnpdsi",
                                          "crspdsi","ag",
                                          "dev","sdev",
                                          "pvi","vote","mhi",
                                          "pvty","college",
                                          "pgwth","p50"))

pairs.pooled <- pooled.all %>% 
  select(variable, value) %>%
  inner_join(shorter.names, by ="variable") %>%
  select(-variable) %>%
  arrange(short.names) %>%
  mutate(id = rep(1:4000,14)) %>%
  spread(short.names, value) %>%
  select(-id)

ggpairs(pairs.pooled, 
        upper=list(continuous = wrap("points", alpha = 0.2, size=0.1)), 
        diag=list(continuous = "density"), 
        lower=list(continuous = wrap("density", size=0.2))) + 
  theme_bw(base_size=8) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

