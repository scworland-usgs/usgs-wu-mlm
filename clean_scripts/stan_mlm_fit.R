
# load packages and set working directory
library(pacman)
pacman::p_load(dplyr, tidyr, rstan, shinystan, ggplot2, GGally, lme4, arm, ggmcmc, stringr, reshape2, magrittr) 

setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/clean_scripts")
load("model.data.Rda")
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')

## set "select" function to dplyr's default
select = dplyr::select

## fraction of data for testing (runs faster)
set.seed(1)
model.data <- sample_frac(model.data,0.05)

# completely pooled regression model----
nrows <- nrow(model.data)
ncols <- ncol(model.data)
X <- model.matrix( ~., model.data[,4:ncols])

stan_data.pooled <- list(y = model.data$wn,
                         X = X,
                         K = ncol(X),
                         N = nrows)


stan.fit.pooled <- stan.regression(wudata, 'simple_reg_stan.stan', stan_data.pooled, 
                                   stan_params=NULL, iter = 500, chains = 1, group.name=NULL)

## extract stan object
fit.pooled_stan <- sflist2stanfit(stan.fit.pooled[1])

## extract data frame with all samples
pooled.all <- stan.fit.pooled[2] %>% data.frame()

## extract summary data frame
pooled.summary <- stan.fit.pooled[3] %>% data.frame() %>% 
  filter(., !grepl("(Intercept)", variable)) %>% 
  mutate(names="pooled") %>% 
  select(variable, names, p97.5, p2.5, p50)

## plot coefficients
ggplot(pooled.summary) + geom_pointrange(aes(variable, p50, ymin=p2.5, ymax=p97.5), size=0.5) + 
  geom_hline(yintercept=0, color="black") + theme_bw() + 
  coord_flip() + ylab("posterior distributions")


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
urban.summary <- stan.fit.urban[3] %>% data.frame() %>% 
  filter(., !grepl("(Intercept)", variable)) %>%
  bind_rows(pooled.summary)

urban.levels <- c("central_metro", "fringe_metro", 
                  "medium_metro","pooled", "small_metro",
                  "micropolitan", "non_core")

urban.summary$names <- factor(urban.summary$names, levels = urban.levels)

ggplot(urban.summary) + geom_pointrange(aes(names, p50, ymin=p2.5, ymax=p97.5), size=0.2) + 
  geom_hline(yintercept=0, color="black") + theme_bw() +
  facet_wrap(~variable, ncol=2) + coord_flip() + 
  xlab(expression(urban %->% rural)) + ylim(-1,1)

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
census.summary <- stan.fit.census[3] %>% data.frame() %>% 
  filter(., !grepl("(Intercept)", variable)) %>%
  bind_rows(pooled.summary)

census.levels <- c("West","Midwest","pooled","South","Northeast")

census.summary$names <- factor(census.summary$names, levels = census.levels)

ggplot(census.summary) + geom_pointrange(aes(names, p50, ymin=p2.5, ymax=p97.5), size=0.2) + 
  geom_hline(yintercept=0, color="black") + theme_bw() +
  facet_wrap(~variable, ncol=2) + coord_flip() + 
  xlab(expression(urban %->% rural)) + ylim(-1,1)



