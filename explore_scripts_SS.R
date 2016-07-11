
# Scripts in response to S.Steinschneider email thread July 2016
pacman::p_load(choroplethr,choroplethrMaps, scales, magrittr, dplyr,
               ggplot2, ggtern)

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
  #group_by(cntyFIPS) %>%
  mutate_each(funs(as.numeric(scale(.))), wn, p) %>%
  #ungroup() %>%
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



# lancover industry + commercial vs residential
lc_data <- read.csv('landcover_2012_county.csv') %>%
  select(region = cntyFIPS,
         com = COMMERC_22_2012, 
         ind = INDUST.MIL_23_2012, 
         res.high = RESID_HI_25_2012,
         res.low = RESID_LO_26_2012,
         urb = URB_OTHER_27_2012) %>%
  mutate(res = res.high + res.low,
         ind.com = ind + com) 

ggplot(lc_data) + geom_point(aes(res_low, com))

# county map
lc_map <- lc_data %>%
  select(region, value=res.low)

county_choropleth(lc_map, title = "landcover low-residential 2012", 
                  num_colors = 1) +
  scale_fill_gradient2(low = "white", 
                       mid = "yellow", 
                       high = "red", 
                       midpoint = 0.2,
                       na.value = "black", 
                       breaks = pretty(lc_map$value, n = 5))


# ternary plots
lc_data2 <- lc_data %>% 
  rename(commercial = com, 
         residential = res, 
         industrial = ind) %>%
  inner_join(wudata.mean)


ggtern(lc_data2,aes(commercial,industrial,residential)) + 
  stat_density_tern(geom='polygon',n = 1000, aes(fill  = ..level.., alpha = ..level..)) +
  scale_fill_gradient(low = "blue",high = "red")  +
  guides(color = "none", fill = "none", alpha = "none") +
  theme_rgbg() + theme_hidetitles() +
  labs(title = "Relative 2012 landcover for U.S. counties") 


ggtern(lc_data2,aes(urb+res.high,res.low,commercial)) + 
  geom_point(alpha=1, aes(color=value)) +  
  scale_color_gradient2(low="khaki1",mid="red",high="blue",midpoint=400) +
  theme_rgbw() + theme_hidetitles() + labs(color="g/p/d") +
  labs(title = "Water use by relative landcover for U.S. counties") 

