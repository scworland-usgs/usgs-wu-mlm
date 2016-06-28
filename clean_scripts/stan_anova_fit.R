
setwd("C:/Users/scworlan/Documents/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")

# load data ----
wudata <- read.csv('cnty_wu_data_complete_march2016.csv') 

wudata2 <- wudata %>%
  dplyr::select(c(wn,census_region,climate_region,CDC_urban,BEA_region, year)) %>%
  mutate(year = as.integer(as.factor(year))) %>%
  mutate(census_region = as.integer(census_region)) %>%
  mutate(CDC_urban = as.integer(CDC_urban)) %>%
  mutate(climate_region = as.integer(climate_region)) %>%
  mutate(BEA_region = as.integer(BEA_region)) %>%
  mutate(wn = replace(wn, wn==0, 1)) %>%
  mutate(wn = log10(wn))

# fit census regions
anova.census <- bayes.anova(wudata2,"wn","census_region",iter=1000,chains=4)

# fit climate regions
anova.climate <- bayes.anova(wudata2,"wn","climate_region",iter=1000,chains=4)

# fit urban classes
anova.urban <- bayes.anova(wudata2,"wn","CDC_urban",iter=1000,chains=4)

# fit years
anova.year <- bayes.anova(wudata2,"wn","year",iter=1000,chains=4)

# BEA regions
anova.bea <- bayes.anova(wudata2,"wn","BEA_region",iter=1000,chains=4)

# combine estimates
anova.combined <- data.frame (rbind(anova.census, anova.climate, anova.bea, anova.urban, anova.year)) %>%
  dplyr::select(c(Source, df), x2.5 = X2.5., x50 = X50., x97.5 = X97.5.)

anova.combined[,3:5] = anova.combined[,3:5]/anova.combined$df
  
# plot the ANOVA results
ggplot(filter(anova.combined,!grepl("within",Source))) + ylab(expression(sigma / df)) +
  geom_pointrange(aes(Source, x50, ymin=x2.5, ymax=x97.5), size=0.5) +
  theme_bw() + coord_flip() + xlab(NULL) + ggtitle ("One-way Bayesian ANOVA") +
  scale_x_discrete(labels=c("census region","climate region","BEA region","urban class","years"))

