
library(dplyr); library(magrittr)

setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/clean_scripts")

## set "select" function to dplyr's default
select = dplyr::select

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')

# set levels for census regions (west ---> east)
census.levels <- c("West","Midwest","South","Northeast")
wudata$census_region <- factor(wudata$census_region, levels = census.levels)

# set levels for urban class (urban ---> rural)
urban.levels <- c("central_metro", "fringe_metro", 
                  "medium_metro", "small_metro",
                  "micropolitan", "non_core")

wudata$CDC_urban <- factor(wudata$CDC_urban, levels = urban.levels)

# prepare model data, comments are after each line
model.data <- select(wudata,census_region,CDC_urban,wn,year,mean_summer_precip_5yr:pop_over50) %>%
  mutate(CDC_urban = as.integer(CDC_urban)) %>% # convert to numeric for stan
  mutate(year = as.integer(as.factor(year))) %>% # convert to numeric for stan
  mutate(census_region = as.integer(census_region)) %>% # convert to numeric for stan
  mutate(wn = replace(wn, wn==0, 1.1)) %>%
  mutate(wn = log10(wn)) %>% # convert response to log scale
  mutate_each(funs(scale), -(census_region:wn)) %>% # scale predictors
  # following landcover classes are highly correlated (~0.9) with "developed" class
  select(-c(landcover_commercial, landcover_residential, landcover_industrial)) %>% 
  select(-length_cross_pdsi_5yr) %>% # correlated with num_cross_pdsi
  select(-pop_under20) %>% # correlated with pop_over50
  select(-c(Percent_HS,Percent_SA)) # keep only one edu metric

save(model.data, file="model.data.Rda")