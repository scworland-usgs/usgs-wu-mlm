
library(geoknife); library(dplyr); library(feather)

summarize = dplyr::summarise
query = geoknife::query

# query available variables in prism dataset
prism = webdata(url='http://cida.usgs.gov/thredds/dodsC/prism')
query(prism,'variables')

# load data
precip <- webdata(list(
  times = as.POSIXct(c('1970-01-01','2010-12-01')),
  url = 'http://cida.usgs.gov/thredds/dodsC/prism',
  variables = 'ppt'))

tmax <- webdata(list(
  times = as.POSIXct(c('1970-01-01','2010-12-01')),
  url = 'http://cida.usgs.gov/thredds/dodsC/prism',
  variables = 'tmx'))

tmin <- webdata(list(
  times = as.POSIXct(c('1970-01-01','2010-12-01')),
  url = 'http://cida.usgs.gov/thredds/dodsC/prism',
  variables = 'tmn'))

# provide FIPS for every county
library(maps) #load fips from maps package
cnty.fips <- stringr::str_pad(as.character(maps::county.fips$fips), pad = '0', width = 5)

# create stencil
all.cnty <- webgeom(geom='sample:Counties', attribute='FIPS', values=cnty.fips)

# download data
precip.job <- geoknife(stencil=all.cnty, fabric=precip, wait = F, email = 'scworland@usgs.gov')
tmax.job <- geoknife(stencil=all.cnty, fabric=tmax, wait = F, email = 'scworland@usgs.gov')
tmin.job <- geoknife(stencil=all.cnty, fabric=tmin, wait = F, email = 'scworland@usgs.gov')

# build seasons data frame
seasons <- data.frame(month=c("Mar","Apr","May",
                              "Jun","Jul","Aug",
                              "Sep","Oct","Nov",
                              "Dec","Jan","Feb"),
                      season=c(rep("spring",3),
                               rep("summer",3),
                               rep("fall",3),
                               rep("winter",3)),
                      stringsAsFactors = F)

# PRECIP ----

# extract precip data
cnty.precip <- result(precip.job) 

# mean summer precip 1970-2010
mean.precip <- cnty.precip %>%
  mutate(year=format(DateTime,'%Y'),
         month = months(DateTime, abbreviate=T)) %>% 
  select(-DateTime,-year,-statistic,-variable) %>%
  melt(., id.vars="month") %>%
  set_colnames(c("month","cnty_fips","ppt")) %>%
  mutate(cnty_fips = as.character(cnty_fips),
         ppt = as.numeric(ppt)) %>%
  left_join(seasons, by="month") %>%
  group_by(season,cnty_fips) %>%
  summarize(summer_mean40yr = mean(ppt, na.rm=T)) %>%
  filter(season=="summer") %>%
  ungroup()

# mean summer precip 2005-2010 minus 40 yr mean
precip05_10 <- cnty.precip %>%
  mutate(year=format(DateTime,'%Y'),
         month = months(DateTime, abbreviate=T)) %>% 
  select(-DateTime,-statistic,-variable) %>%
  melt(., id.vars=c("year","month")) %>%
  set_colnames(c("year","month","cnty_fips","ppt")) %>%
  mutate(cnty_fips = as.character(cnty_fips),
         ppt = as.numeric(ppt)) %>%
  filter(year %in% c(as.character(2005:2010))) %>%
  left_join(seasons, by="month") %>%
  group_by(season,cnty_fips) %>%
  summarize(summer_mean05_10 = mean(ppt, na.rm=T)) %>%
  filter(season=="summer") %>%
  ungroup() %>%
  select(-season) %>%
  left_join(mean.precip, by="cnty_fips") %>%
  mutate(ppt_diff=summer_mean05_10-summer_mean40yr) %>%
  ungroup()

# write_feather(precip05_10, "data/precip_diff_40yr.feather")
precip05_10 <- read_feather("data/precip_diff_40yr.feather")

mean.precip.map <- precip05_10 %>%
  select(region=cnty_fips, value=ppt_diff) %>%
  mutate(region=as.integer(region),
         value=as.numeric(value))
  
county_choropleth(mean.precip.map , num_colors = 8, 
                  title="2005-2010 summer precip - 40 yr summer precip") +
  scale_fill_brewer(name="ppt diff", palette='YlGnBu', drop=FALSE)
  
# TEMP ----

# extract precip data
cnty.temp <- result(tmax.job) 

# mean max summer temp 1970-2010
mean.tmax <- cnty.temp %>%
  mutate(year=format(DateTime,'%Y'),
         month = months(DateTime, abbreviate=T)) %>% 
  select(-DateTime,-year,-statistic,-variable) %>%
  melt(., id.vars="month") %>%
  set_colnames(c("month","cnty_fips","mxtmp")) %>%
  mutate(cnty_fips = as.character(cnty_fips),
         mxtmp = as.numeric(mxtmp)) %>%
  left_join(seasons, by="month") %>%
  group_by(season,cnty_fips) %>%
  summarize(summer_mean40yr = mean(mxtmp, na.rm=T)) %>%
  filter(season=="summer") %>%
  ungroup()

# mean max summer temp 2005-2010 minus 40 yr mean
tmax05_10 <- cnty.temp %>%
  mutate(year=format(DateTime,'%Y'),
         month = months(DateTime, abbreviate=T)) %>% 
  select(-DateTime,-statistic,-variable) %>%
  melt(., id.vars=c("year","month")) %>%
  set_colnames(c("year","month","cnty_fips","mxtmp")) %>%
  mutate(cnty_fips = as.character(cnty_fips),
         mxtmp = as.numeric(mxtmp)) %>%
  filter(year %in% c(as.character(2005:2010))) %>%
  left_join(seasons, by="month") %>%
  group_by(season,cnty_fips) %>%
  summarize(summer_mean05_10 = mean(mxtmp, na.rm=T)) %>%
  filter(season=="summer") %>%
  ungroup() %>%
  select(-season) %>%
  left_join(mean.tmax, by="cnty_fips") %>%
  mutate(tmax_diff=summer_mean05_10-summer_mean40yr) %>%
  ungroup()

# write_feather(tmax05_10, "data/tmax_diff_40yr.feather")
tmax05_10 <- read_feather("data/tmax_diff_40yr.feather")

mean.tmax.map <- tmax05_10 %>%
  select(region=cnty_fips, value=tmax_diff) %>%
  mutate(region=as.integer(region),
         value=as.numeric(value))

county_choropleth(mean.tmax.map, num_colors = 8, 
                  title="2005-2010 summer max temp - 40 yr summer max temp") +
  scale_fill_brewer(name="tmax diff", palette='YlOrRd', drop=FALSE)


