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

# short names
short.names <- data.frame(Parameter = names(model.data[,5:20]), 
                          short.names = c("mean-ppt","max-temp","min-pdsi",
                                          "cross-pdsi","agriculture","urban",
                                          "suburban","pvi","voting-pop",
                                          "mhi","poverty","BA","HS",
                                          "pop","popGrowth","pop50"))

# correlation matrix for predictors
cormat <- cor(model.data[,4:n])
corrplot::corrplot(cormat,method="ellipse", diag=F, type = "lower")

# density plot of covariates
dens <- dplyr::select(wudata, census_region, year, mean_summer_precip_5yr:pop_over50) %>%
  mutate(landcover_semi_dev = replace(landcover_semi_dev, landcover_semi_dev==0, 0.01)) %>%
  mutate(num_cross_pdsi_5yr = replace(num_cross_pdsi_5yr,num_cross_pdsi_5yr==0, 1)) %>%
  mutate_each(funs(log10), c(pop, landcover_developed, landcover_semi_dev, num_cross_pdsi_5yr)) %>%
  mutate_each(funs(scale), -c(census_region,year)) %>%
  dplyr::select(-c(landcover_commercial, landcover_residential, landcover_industrial, landcover_water)) %>% 
  dplyr::select(-length_cross_pdsi_5yr) %>% # correlated with num_cross_pdsi
  dplyr::select(-pop_under20) %>% # correlated with pop_over50
  dplyr::select(-Percent_SA) %>% # correlated with % BA
  #rename(avppt = mean_summer_precip_5yr,mxtemp=mean_max_temp_5yr,mnpdsi=mean_min_pdsi_5yr,
         #crspdsi=num_cross_pdsi_5yr, ag=landcover_agriculture, dev=landcover_developed,
         #semidev=landcover_semi_dev,vote=voting_pop, pov=poverty, BA=Percent_BA,HS=Percent_HS,
         #popgrow=popGrowth_5yr,pop50=pop_over50) %>%
  melt(id.vars=c("census_region","year")) 

p.dens <- ggplot(dens,aes(x = value, group=factor(census_region), fill = factor(census_region))) + 
  geom_density(alpha = 0.5) + theme_bw() +
  scale_fill_brewer(palette="YlGnBu",name=NULL) +
  facet_wrap(~variable, scales="free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p.dens


# randomly subset 5% of rows to test models on smaller data set
set.seed(1)
model.data2 <- sample_frac(model.data,0.05)

# lmer models ----

# climate region

## build formula for climate regions
region = "climate_region"
lmer.formula.region <- reformulate(paste0(colnames(model.data[,5:n])," + 
                                          (1 + ",colnames(model.data[,5:n]), "|",
                                          region,")",collapse="+"), response = 'wn')

## fit model
lmer.fit.region <- lmer(lmer.formula.region, data = model.data)

## extract coefficients and add region names  
cf.region = coef(lmer.fit.region)$climate_region
cf.region$climate_region <- unique(wudata$climate_region)

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

## build formula for year
lmer.formula.year <- reformulate(paste0(colnames(model.data[,5:n])," + 
                                          (1 + ",colnames(model.data[,5:n]), "|",
                                          "year",")",collapse="+"), response = 'wn')

## fit model
lmer.fit.year <- lmer(lmer.formula.year, data = model.data)

## extract coefficients and add region names  
cf.year = coef(lmer.fit.year)$year
cf.year$year <- c("1985","1990","1995","2000","2005","2010")

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
X2 <- model.matrix( ~., model.data2[,5:n])

# completely pooled
## lm 
lm.pooled <- lm(model.data$wn~X)$coef %>% data.frame() 
lm.pooled %<>% mutate(Parameter=row.names(lm.pooled))
colnames(lm.pooled)[1]="value.lm"
lm.pooled %<>% filter(value.lm < 1.5) %>% 
  mutate(Parameter = substring(Parameter, 2))

## stan
stan_data.pooled <- list(y = model.data$wn,
                         X = X,
                         K = ncol(X),
                         N = nrow(model.data))

stan_model.pooled <- stan_model('wu_simple_reg.stan', model_name = "pooled mlm wu regression")
stan.fit.pooled <- sampling(stan_model.pooled, stan_data.pooled, iter = 2000, chains = 1)

g.pooled <- ggs(stan.fit.pooled,family="beta") %>%
  mutate(num = as.numeric(gsub("[^\\d]+", "", Parameter, perl=TRUE))) %>%
  mutate(Parameter =  colnames(X)[num]) %>% 
  group_by(Parameter) %>%
  summarise(p95.pooled=quantile(value, probs=0.975),
            p05.pooled=quantile(value, probs=0.025),
            value.pooled=mean(value)) %>%
  filter(value.pooled < 1.5) %>%
  inner_join(lm.pooled, by="Parameter")

ggplot(g.pooled) + geom_pointrange(aes(Parameter, value.pooled, ymin=p05.pooled, ymax=p95.pooled), size=0.3) + 
  geom_point(aes(Parameter, value.lm), color="red") +
  geom_hline(yintercept=0, color="black") + coord_flip() +
  theme_bw() + ylab("posterior distribution with 95% HDI")

# Years as levels ----

# regression
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
            p05=quantile(value, probs=0.05),
            value=mean(value)) %>%
  filter(value < 1.5) %>%
  filter(Parameter != "CDC_urban") %>%
  inner_join(cfym, by=c("Parameter", "year")) %>%
  inner_join(g.pooled, by = "Parameter") %>%
  inner_join(short.names, by = "Parameter") %>%
  mutate(sign = ifelse(value.x>0, 1, 0)) 
                             
# Main year plot
ggplot(g.year2) + 
  geom_rect(aes(ymin=p05.pooled, xmin=1980, ymax=p95.pooled, xmax=2015), fill = "steelblue",alpha=0.1) +
  geom_pointrange(aes(as.numeric(year), value.x, ymin=p05, ymax=p95), fill="black",size=0.05) + 
  #geom_point(aes(year, value.y), color="red", size=1) +
  geom_hline(yintercept=0, color="black") +
  #scale_fill_manual(values = c("white","black"), lab=c("negative","positive")) +
  labs(fill="sign") +
  # geom_hline(aes(yintercept=p05.pooled), color="blue", linetype="dashed") +
  # geom_hline(aes(yintercept=p95.pooled), color="blue", linetype="dashed") +
  #geom_ribbon(aes(group=1, x=year, ymin=p05.pooled, ymax=p95.pooled), alpha=0.3) +
  facet_wrap(~short.names, ncol=4)  + ylim(c(-0.2,0.25)) + coord_cartesian(xlim=c(1982, 2012)) +
  scale_x_continuous(breaks=c(1985,1990,1995,2000,2005,2010), labels=c("85","90","95","00","05","10")) +
  theme_bw(base_size=10) + xlab("year") + ylab("posterior distribution with 95% HDI") +
  theme(strip.text.x = element_text(size = 10))

## point range + lmer estimates
# ggplot(g.year2) + geom_pointrange(aes(factor(year), value.x, ymin=p05, ymax=p95), size=0.3) + 
#   geom_point(aes(factor(year), value.y), color="red", size=1) +
#   geom_hline(yintercept=0, color="black") +
#   facet_wrap(~Parameter) + ylim(c(-0.2,0.2)) +
#   theme_bw() + xlab("year") + ylab("posterior distribution with 95% HDI")

# Climate Regions as levels ----

stan_data.regions <- list(y = model.data$wn,
                          climate_region = model.data$climate_region,
                          X = X,
                          K = ncol(X),
                          N = nrow(model.data),
                          J = length(unique(model.data$climate_region)),
                          nu = 2)

stan_model.climate <- stan_model('wu_mlm_climate_region.stan', model_name = "mlm wu climate region")

stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')
fit.climate.regions <- sampling(stan_model.climate, stan_data.regions, pars = stan_params, iter = 2000, chains = 1)
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
            p05=quantile(value, probs=0.05),
            value=mean(value)) %>%
  filter(value < 1.5) %>%
  filter(Parameter != "CDC_urban") %>%
  inner_join(cfrm, by=c("Parameter", "climate_region")) %>%
  inner_join(g.pooled, by = "Parameter") %>%
   mutate(sign = ifelse(value.x>0, 1, 0))


ggplot(g.cens.regions2) + 
  geom_pointrange(aes(cens.regions, value.x, ymin=p05, ymax=p95, fill=factor(sign)), shape=21,size=0.3) + 
  #geom_point(aes(cens.regions, value.y), color="red", size=1) +
  #geom_hline(yintercept=0, color="black") +
  scale_fill_manual(values = c("white","black"), lab=c("negative","positive")) +
  labs(fill="sign") +
  geom_hline(aes(yintercept=p05.pooled), color="blue", linetype="dashed") +
  geom_hline(aes(yintercept=p95.pooled), color="blue", linetype="dashed") +
  facet_wrap(~Parameter) + coord_flip() + ylim(c(-0.2,0.25)) +
  theme_bw() + xlab("census region") + ylab("posterior distribution with 95% HDI")

## point range + lmer estimates
ggplot(g.regions2) + 
  geom_pointrange(aes(climate_region, value.x, ymin=p05, ymax=p95, fill=factor(sign)), shape=21, size=0.3) + 
  #geom_point(aes(climate_region, value.y), color="red", size=1) +
  #geom_hline(yintercept=0, color="black") +
  scale_fill_manual(values = c("white","black"), lab=c("negative","positive")) +
  labs(fill="sign") +
  geom_hline(aes(yintercept=p05.pooled), color="blue", linetype="dashed") +
  geom_hline(aes(yintercept=p95.pooled), color="blue", linetype="dashed") +
  facet_wrap(~Parameter) + ylim(c(-0.5,0.5)) + coord_flip() +
  theme_bw() + xlab("climate region") + ylab("posterior distribution with 95% HDI")

# Census Regions as levels ----
model.data3 <- dplyr::select(wudata,cntyFIPS,census_region,year:MSAclass, CDC_urban) %>%
  mutate(CDC_urban = as.integer(CDC_urban)) %>%
  mutate(year = as.integer(as.factor(year))) %>%
  mutate(census_region = as.integer(census_region)) %>%
  mutate(wn = replace(wn, wn==0, 1)) %>%
  mutate(wn = log10(wn)) %>%
  mutate_each(funs(scale), -(cntyFIPS:wn)) %>%
  dplyr::select(-c(landcover_commercial, landcover_residential, landcover_industrial, landcover_water)) %>% 
  dplyr::select(-length_cross_pdsi_5yr) %>% # correlated with num_cross_pdsi
  dplyr::select(-pop_under20) %>% # correlated with pop_over50
  dplyr::select(-MSAclass) %>% # correlated with CDC_urban
  dplyr::select(-Percent_SA) # correlated with % BA

## lmer census
region = "census_region"
lmer.formula.cens.region <- reformulate(paste0( paste0(colnames(model.data3[,5:n])," + 
                                                      (1 + ",colnames(model.data3[,5:n]), "|",
                                                      region,")",collapse="+")), response = 'wn')

## fit model
lmer.fit.cens.region <- lmer(lmer.formula.cens.region, data = model.data3)



#ggplot(model.data3, aes(mhi, poverty)) + geom_point(aes(color=factor(census_region)),alpha=0.2) +
#  facet_wrap(~census_region)

## extract coefficients and add region names  
cf.cens.region = coef(lmer.fit.cens.region)$census_region
cf.cens.region$cens.region <- c("South","West","Northeast","Midwest")

## long format for plot
cfcrm <- melt(cf.cens.region) 
cfcrm <- inner_join(cfcrm,variable_type, by="variable")
colnames(cfcrm)[1:2] <- c("cens.regions","Parameter")
cfcrm <- cfcrm %>% mutate(Parameter = as.character(Parameter)) %>%
  mutate(cens.regions = as.character(cens.regions))

## STAN
stan_data.cen.regions <- list(y = model.data3$wn,
                          census_region = model.data3$census_region,
                          X = X,
                          K = ncol(X),
                          N = nrow(model.data3),
                          J = length(unique(model.data3$census_region)),
                          nu = 2)

stan_model.census <- stan_model('wu_mlm_census_region.stan', model_name = "mlm wu census region")

stan_params <- c('gamma', 'beta', 'sigma', 'tau', 'z')
fit.cens.regions <- sampling(stan_model.census, stan_data.cen.regions, pars = stan_params, iter = 2000, chains = 1)


g.cens.regions <- ggs(fit.cens.regions,family="beta") %>% 
  mutate(num = as.numeric(gsub("[^\\d]+", "", g.cens.regions$Parameter, perl=TRUE))) %>%
  mutate(cens.regions_index = as.numeric(substr(as.character(g.cens.regions$num),1,1))) %>%
  mutate(coef_index = as.numeric(substring(as.character(g.cens.regions$num), 2))) %>%
  mutate(cens.regions = as.character(unique(wudata$census_region)[g.cens.regions$cens.regions_index])) %>%
  mutate(Parameter =  colnames(X)[g.cens.regions$coef_index])


g.cens.regions2 <- g.cens.regions %>% group_by(Parameter, cens.regions) %>%
  summarise(p95=quantile(value, probs=0.975),
            p05=quantile(value, probs=0.025),
            value=mean(value)) %>%
  filter(value < 1.5) %>%
  filter(Parameter != "CDC_urban") %>%
  inner_join(cfcrm, by=c("Parameter", "cens.regions")) %>%
  inner_join(g.pooled, by = "Parameter") %>%
  inner_join(short.names, by = "Parameter") %>%
  mutate(sign = ifelse(value.x>0, 1, 0))

ggplot(g.cens.regions2) + 
  geom_ribbon(aes(group=1, x=cens.regions, ymin=p05.pooled, ymax=p95.pooled), alpha=0.3, fill = "steelblue")  +
  geom_pointrange(aes(cens.regions, value.x, ymin=p05, ymax=p95), size=0.1) + 
  #geom_point(aes(cens.regions, value.y), color="red", size=1) +
  geom_hline(yintercept=0, color="black") +
  #scale_fill_manual(values = c("white","black"), lab=c("negative","positive")) +
  labs(fill="sign") +
  #geom_hline(aes(yintercept=p05.pooled), color="blue", linetype="dashed") +
  #geom_hline(aes(yintercept=p95.pooled), color="blue", linetype="dashed") +
  facet_wrap(~short.names, ncol=2) + coord_flip() + ylim(c(-0.2,0.25)) +
  theme_bw(base_size=10) + xlab("census region") + ylab("posterior distribution with 95% HDI") +
  theme(strip.text.x = element_text(size = 10))



### Inverse Wishart model !!! I cannot get it to run. Something about covariance matrix
# stan_data.cen.regions.invw <- list(y = model.data3$wn,
#                                   census_region = model.data3$census_region,
#                                   X = X,
#                                   K = ncol(X),
#                                   N = nrow(model.data3),
#                                   J = length(unique(model.data3$census_region)),
#                                   W = diag(ncol(X)))
# 
# stan_model.census_invw <- stan_model('wu_mlm_census_region_inv_wishart.stan', model_name = "mlm wu census region invw")
# 
# fit.cens.regions_invw <- sampling(stan_model.census_invw, stan_data.cen.regions.invw, iter = 2000, chains = 1)

# lmer census:years
region = "census_region:year"
lmer.form.cen.year <- reformulate(paste0( paste0(colnames(model.data3[,5:n])," + 
                                                       (1 + ",colnames(model.data3[,5:n]), "|",
                                                       region,")",collapse="+")), response = 'wn')

lmer.cen.years <- lmer(lmer.form.cen.year, data=model.data3)

lmer.cen.year.cf <- coef(lmer.cen.years)$'census_region:year' %>% data.frame() %>% 
  mutate(year = rep(unique(wudata$year),1, each=4)) %>%
  mutate(census.region = rep(unique(wudata$census_region),6)) %>% 
  melt(id.vars=c("census.region","year")) %>% filter(value<1.9) %>%
  filter(variable != "CDC_urban") 

ggplot(lmer.cen.year.cf) + geom_point(aes(year,value, color = factor(census.region))) +
  geom_line(aes(year,value, color = factor(census.region))) + facet_wrap(~variable) +
  labs(color="Census Region") + theme_bw(base_size=12) + geom_hline(yintercept=0)

