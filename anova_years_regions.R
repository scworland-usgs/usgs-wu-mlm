library(dplyr)
library(tidyr)
library(rstan)
library(shinystan)
library(ggplot2)
library(GGally)
library(arm)
library(RColorBrewer)
library(ggmcmc)
library(stringr)
library(reshape2)
library(magrittr)

#set working directory
setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv') 

wudata2 <- wudata %>%
  mutate(year = as.integer(as.factor(year))) %>%
  mutate(census_region = as.integer(census_region)) %>%
  mutate(wn = replace(wn, wn==0, 1)) %>%
  mutate(wn = log10(wn))

# Bayesian ANOVA for census regions ---
anova.list.region <- list(N = nrow(wudata2), 
                          y = wudata2$wn, 
                          group = wudata2$census_region,
                          J=length(unique(wudata2$census_region)))

anova_stan.region.fit. <- stan(file='anova_stan.stan', data=anova.list.region,
                              iter=1000, chains=4)

anova.region.df <- summary(anova_stan.fit, c("s_y", "s_a"))$summary 

anova.region.df <- data.frame(anova.region.df,
                              Source = factor(rownames(anova.region.df),
                                       levels = rownames(anova.region.df),
                                       labels = c("error", "census_regions")),
                              df = with(anova.list.region, c(N-J, J-1)))

# Bayesian ANOVA for years----
anova.list.year <- list(N = nrow(wudata2), 
                        y = wudata2$wn, 
                        year = wudata2$year,
                        J=length(unique(wudata2$year)))

anova_stan.year.fit <- stan(file='anova_stan.stan', data=anova.list.year,
                            iter=1000, chains=4)

anova.year.df <- summary(anova_stan.year.fit, c("s_y", "s_a"))$summary

anova.year.df <- data.frame(anova.year.df,
                            Source = factor(rownames(anova.year.df),
                                            levels = rownames(anova.year.df),
                                            labels = c("error", "years")),
                            df = with(anova.list.year, c(N-J, J-1)))

# merge and plot
anova.combined <- data.frame(type = c("error","census_region","year"),
                             x50 = c(anova.df$X50.[1],
                                      anova.df$X50.[2],
                                      anova.year.df$X50.[2]),
                             x2.5 = c(anova.df$X2.5.[1],
                                      anova.df$X2.5.[2],
                                      anova.year.df$X2.5.[2]),
                             x97.5 = c(anova.df$X97.5.[1],
                                      anova.df$X97.5.[2],
                                      anova.year.df$X97.5.[2]))

ggplot(anova.combined) + ylab(NULL) + xlab("Source of variation") +
  geom_pointrange(aes(type, x50, ymin=x2.5, ymax=x97.5), size=0.5) +
  theme_bw() 

