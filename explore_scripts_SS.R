
# Scripts in response to S.Steinschneider email thread July 2016
pacman::p_load(choroplethr,choroplethrMaps, scales, magrittr, dplyr,
               ggplot2)

# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm")
wudata <- read.csv('cnty_wu_data_complete_march2016.csv')

# (1) make a map of 1985-2010 avg Wn for each county
wudata.mean <- wudata %>% 
  group_by(cntyFIPS) %>%
  summarize(value = mean(wn)) %>%
  filter(value < 1000) %>%
  rename(region=cntyFIPS)

county_choropleth(wudata.mean)

# (2) calculate slopes for standardized precip vs wn for each county and year
p_wn <- wudata %>%   
  mutate(wn = replace(wn, wn==0, 1.1)) %>%
  mutate(wn = log10(wn)) %>%
  select(cntyFIPS,wn,mean_summer_precip_5yr) %>%
  rename(p = mean_summer_precip_5yr) %>%
  group_by(cntyFIPS) %>%
  mutate_each(funs(as.numeric(scale(.))), wn, p) %>%
  ungroup() %>%
  na.omit()

p_wn.cor <- p_wn %>%
  group_by(cntyFIPS) %>%
  summarize(value = cor(wn,p)) %>%
  rename(region=cntyFIPS)

p_wn.slope <- p_wn %>%
  group_by(cntyFIPS) %>%
  do(mod = lm(wn ~ p, data = .)) %>%
  mutate(value = summary(mod)$coeff[2]) %>%
  select(-mod) %>%
  rename(region=cntyFIPS) 
  

county_choropleth(p_wn.slope, title = "slopes for wn ~ precip", 
                  num_colors = 1) +
  scale_fill_gradient2(low = muted("red"), 
                       mid = "white", 
                       high = muted("blue"), 
                       midpoint = 0,
                       na.value = "black", 
                       breaks = pretty(p_wn.slope$value, n = 10))

p_wn_regions <- p_wn.slope %>%
  rename(cntyFIPS=region) %>%
  inner_join(wudata, by = "cntyFIPS") 

  
ggplot(p_wn_regions) + 
  geom_boxplot(aes(census_region, value, fill = CDC_urban)) +
  geom_violin(aes(census_region, value), fill = "grey", alpha=0.3) + 
  theme_bw() + ggtitle("slopes for wn~precip") + xlab(NULL) +
  ylab("slope")






