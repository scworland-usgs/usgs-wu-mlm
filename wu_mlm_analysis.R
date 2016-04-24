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
library(magrittr)

setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')

# prepare model data
model.data <- dplyr::select(wudata,cntyFIPS,climate_region,year:MSAclass, CDC_urban) %>%
  mutate(CDC_urban = as.integer(CDC_urban)) %>%
  mutate(year = as.integer(as.factor(year))) %>%
  mutate(climate_region = as.integer(climate_region)) %>%
  mutate(wn = replace(wn, wn==0, 1)) %>%
  mutate(wn = log10(wn)) %>%
  mutate_each(funs(scale), -(cntyFIPS:wn)) %>%
  dplyr::select(-c(landcover_commercial, landcover_residential, landcover_industrial, landcover_water)) %>% 
  dplyr::select(-length_cross_pdsi_5yr) %>% # correlated with num_cross_pdsi
  dplyr::select(-pop_under20) %>% # correlated with pop_over50
  dplyr::select(-MSAclass) %>% # correlated with CDC_urban
  dplyr::select(-Percent_SA) # correlated with % BA


# number of columns
n <- ncol(model.data)

# manually classify variables as evironmental or socio-economic
variable_type <- data_frame(variable = names(model.data[,5:n]),
                            type = c(rep("environmental",4),
                                     rep("socio-economic",13)))


# correlation matrix for predictors
cormat <- cor(model.data[,4:n])
corrplot::corrplot(cormat,method="ellipse", diag=F, type = "lower")

# randomly subset 5% of rows to test models on smaller data set
set.seed(1)
model.data2 <- sample_frac(model.data,0.05)

# lmer models ----

# climate region

## build formula for climate regions
region = "climate_region"
lmer.formula.region <- reformulate(paste0("(1 | ", region, ") + ", 
                                   paste0(colnames(model.data[,5:n])," + 
                                          (1 + ",colnames(model.data[,5:n]), "|",
                                          region,")",collapse="+")), response = 'wn')

## fit model
lmer.fit.region <- lmer(lmer.formula.region, data = model.data)

## extract coefficients and add region names  
cf.region = coef(lmer.fit.region)$climate_region
cr <- data_frame(name = unique(wudata$climate_region),
                 level = unique(model.data$climate_region)) %>%
                 mutate(level = as.character(level))

cf.region$level <- rownames(cf.region)
cf.region <- inner_join(cf.region,cr,by="level") %>% 
  dplyr::select(name, mean_summer_precip_5yr:CDC_urban)

## long format for plot
cfrm <- melt(cf.region)
cfrm <- inner_join(cfrm,variable_type, by="variable")
colnames(cfrm)[1:2] <- c("climate_region","Parameter")

## facet plot
ggplot(cfrm) + geom_point(aes(value, name), shape= 21, size = 3, color="black", fill="dodgerblue") + 
  geom_vline(xintercept=0, color="black") +
  facet_wrap(~Parameter) +
  theme_bw()

# year

## build formula for climate regions
lmer.formula.year <- reformulate(paste0("(1 | ", "year", ") + ", 
                                   paste0(colnames(model.data[,5:n])," + 
                                          (1 + ",colnames(model.data[,5:n]), "|",
                                          "year",")",collapse="+")), response = 'wn')

## fit model
lmer.fit.year <- lmer(lmer.formula.year, data = model.data)

## extract coefficients and add region names  
cf.year = coef(lmer.fit.year)$year
cf.year$year <- rownames(cf.year)

## long format for plot
cfym <- melt(cf.year)
cfym <- inner_join(cfym,variable_type, by="variable")
colnames(cfym)[2] <- "Parameter"

## facet plot
ggplot(cfym) + geom_point(aes(year, value), shape= 21, size = 3, color="black", fill="dodgerblue") + 
  geom_hline(yintercept=0, color="black") +
  facet_wrap(~Parameter) +
  theme_bw()

# Bayesian fit ----

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

X <- model.matrix( ~., model.data[,5:n])

# Years as levels ----
stan_data.year <- list(y = model.data$wn,
                  year = model.data$year,
                  X = X,
                  K = ncol(X),
                  N = nrow(model.data),
                  J = length(unique(model.data$year)),
                  nu = 2)

stan_model.year <- stan_model('wu_mlm_regression.stan', model_name = "mlm wu year regression")

stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')
fit.year <- sampling(stan_model.year, stan_data.year, pars = stan_params,iter = 2000, chains = 1)

# posterior distributions for year mlm
g.year <- ggs(fit.year,family="beta") %>% 
  mutate(num = as.numeric(gsub("[^\\d]+", "", g.year$Parameter, perl=TRUE))) %>%
  mutate(year_index = as.numeric(substr(as.character(g.year$num),1,1))) %>%
  mutate(coef_index = as.numeric(substring(as.character(g.year$num), 2))) %>%
  mutate(year = as.character(unique(wudata$year)[g.year$year_index])) %>%
  mutate(Parameter =  colnames(X)[g.year$coef_index])

# boxplot of distributions
ggplot(g.year) + geom_boxplot(aes(factor(year), value), color="black", fill="dodgerblue",outlier.shape=NA) + 
  geom_hline(yintercept=0, color="black") +
  facet_wrap(~Parameter) + ylim(c(-0.25,0.25)) +
  theme_bw()

# mean, 5%, and 95% for stan estimates and lmer pt estimates
g.year2 <- g.year %>% group_by(Parameter, year) %>%
  summarise(p95=quantile(value, probs=0.95),
            p05=quantile(value, probs=0.5),
            value=mean(value)) %>%
  filter(value < 1.5) %>%
  inner_join(cfym, by=c("Parameter", "year"))

## point range + lmer estimates
ggplot(g.year2) + geom_pointrange(aes(factor(year), value.x, ymin=p05, ymax=p95), size=0.3) + 
  geom_point(aes(factor(year), value.y), color="red", size=1) +
  geom_hline(yintercept=0, color="black") +
  facet_wrap(~Parameter) + ylim(c(-0.2,0.2)) +
  theme_bw() + xlab("year") + ylab("posterior distribution with 95% HDI")


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
fit.regions <- sampling(stan_model, stan_data.regions, pars = stan_params, iter = 2000, chains = 1)
launch_shinystan(fit.regions)

# posterior distributions for regions mlm
g.regions <- ggs(fit.regions,family="beta") %>% 
  mutate(num = as.numeric(gsub("[^\\d]+", "", g.regions$Parameter, perl=TRUE))) %>%
  mutate(regions_index = as.numeric(substr(as.character(g.regions$num),1,1))) %>%
  mutate(coef_index = as.numeric(substring(as.character(g.regions$num), 2))) %>%
  mutate(regions = as.character(unique(wudata$regions)[g.regions$regions_index])) %>%
  mutate(Parameter =  colnames(X)[g.regions$coef_index])

# boxplot of distributions
ggplot(g.regions[,c(3,4,8)]) + ylim(c(-1,1)) +
  geom_boxplot(aes(x=climate_region,y=value), fill="grey50", outlier.shape=NA) + 
  geom_hline(yintercept = 0) + ggtitle("Posterior estimates for climate regions") +
  coord_flip() + theme_bw(base_size=11) + labs(x=NULL, fill="year") + facet_wrap(~Parameter)

# mean, 5%, and 95% for stan estimates and lmer pt estimates
g.regions2 <- g.regions %>% group_by(Parameter, climate_region) %>%
  summarise(p95=quantile(value, probs=0.95),
            p05=quantile(value, probs=0.5),
            value=mean(value)) %>%
  filter(value < 1.5) %>%
  inner_join(cfrm, by=c("Parameter", "climate_region"))

## point range + lmer estimates
ggplot(g.regions2) + geom_pointrange(aes(climate_region, value.x, ymin=p05, ymax=p95), size=0.3) + 
  geom_point(aes(climate_region, value.y), color="red", size=1) +
  geom_hline(yintercept=0, color="black") +
  facet_wrap(~Parameter) + ylim(c(-0.4,0.4)) + coord_flip() +
  theme_bw() + xlab("climate_region") + ylab("posterior distribution with 95% HDI")






