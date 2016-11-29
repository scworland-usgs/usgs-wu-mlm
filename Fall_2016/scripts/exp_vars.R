
# laod libraries
library(pacman)
pacman::p_load(ggplot2,dplyr,reshape2,xtable,magrittr,
               stringr,readxl,maps,choroplethr,ggmap,purrr, 
               viridis,acs,feather,gridExtra,corrr)

# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/Fall_2016")

# modify or set default functions
read_excel <-  function(...) {
  quiet_read <- purrr::quietly(readxl::read_excel)
  out <- quiet_read(...)
  if(length(c(out[["warnings"]], out[["messages"]])) == 0)
    return(out[["result"]])
  else readxl::read_excel(...)
}

summarize = dplyr::summarize
rename = dplyr::rename
select <- dplyr::select

# water use data ----

# build state name and abb data.frame
states <- data.frame(state.abb,state=state.name, stringsAsFactors = F) %>%
  bind_rows(data.frame(state.abb="DC", state="district of columbia", 
                       stringsAsFactors = F))

# url for wudata file
wudata.file <- "http://water.usgs.gov/watuse/data/2010/usco2010.txt"

# load datafile and add columns
wudata <- read.delim(wudata.file, stringsAsFactors=F) %>%
  setNames(str_to_lower(names(.))) %>%
  filter(!state %in% c("AK","HI","PR","VI")) %>%
  select(cnty_fips=fips, state.abb=state, county, year, cnty_pop=tp.totpop,
         pop_served=ps.topop, wthdrw_gw=ps.wgwfr, wthdrw_sw=ps.wswfr, 
         wthdrw_tw=ps.wfrto) %>%
  left_join(states, by="state.abb") %>%
  mutate(cnty_fips = str_pad(cnty_fips,5, pad = "0"),# add leading zero
         county = str_replace_all(county, fixed(" County"), ""), # remove word "County"
         cnty_pop = cnty_pop*1000, # convert population to raw pop
         pop_served = pop_served*1000,
         prop_sw=wthdrw_sw/wthdrw_tw)

# grouping variables ----

# load typology file into R
cnty_typology <- read_excel("data/cnty_typology_2004.xls", sheet = 2, col_names = T) %>%
  select(cnty_fips=FIPSTXT,rur2urb=rururb2003,econdep,retire) %>%
  mutate(cnty_fips=as.numeric(cnty_fips) %>%
           round(.,0) %>%
           as.character() %>%
           str_pad(5, pad = "0")) %>%
  mutate_each(funs(as.character))

# add more meaningful descriptions
## urban continuum
rur2urb_des <- data.frame(
  rur2urb = as.character(c(1:9)),
  rur2urb_des = c("metro_large", # pop >= 1e6
                  "metro_med", # pop 250,000 to 1e6
                  "metro_small", # pop < 250,000
                  "urb_large_adj", # pop >= 20,000, metro adjacent 
                  "urb_large_det", # pop >= 20,000, metro detached
                  "urb_small_adj", # pop 2500-20,000, metro adjacent
                  "urb_small_det", # pop 2500-20,000, metro detached
                  "rural_adj", # pop < 2500, metro adjacent
                  "rural_det"), # pop < 2500, metro detached
  stringsAsFactors = F)

## economic dependencies
econdep_des <- data.frame(
  econdep = as.character(c(1:6)),
  econdep_des = c("farming", # dependent on farming
                  "mining", # dependent on mining
                  "manufacuring", # dependent on manufactoring
                  "government", # dependent on federal and/or state govt
                  "services", # dependent on services
                  "nonspecialized"), # nonspecialized
  stringsAsFactors = F)

cnty_typology %<>% 
  left_join(rur2urb_des, by="rur2urb") %>%
  left_join(econdep_des, by="econdep") %>%
  select(cnty_fips, rur2urb=rur2urb_des, econdep=econdep_des,retire)

## climate regions
climate_region <- read.csv("data/climate_regions.csv", header=T, stringsAsFactors = F) %>%
  rename(state.abb=state)

# explanatory variables ----

## mean summer precip 1970-2010
precip05_10 <- read_feather("data/precip_diff_40yr.feather") %>%
  select(cnty_fips,ppt_0510=summer_mean05_10,
         ppt_40=summer_mean40yr,ppt_diff)

## mean summer temp 1970-2010
tmax05_10 <- read_feather("data/tmax_diff_40yr.feather") %>%
  select(cnty_fips,tmax_0510=summer_mean05_10,
         tmax_40=summer_mean40yr,tmax_diff)

## water yield
wyield <- read.csv("data/VIC_Q_by_CountyFIPS.csv", header=T, 
                   colClasses = c("character","numeric","numeric")) %>%
  mutate(cnty_fips=str_pad(cnty_fips, pad = '0', width = 5))

## cooks partisan voting index
cnty_pvi <- read.csv("data/cnty_pvi.csv", stringsAsFactors = F) %>%
  select(cnty_fips,cnty.pvi) %>%
  mutate(cnty_fips = as.character(cnty_fips) %>%
           str_pad(5, pad = "0"))

## county land area in sq miles
cnty.area <- read_excel("data/cnty_area.xls") %>%
  select(cnty_fips=STCOU,cnty_area=LND110200D)

# make geo object of counties
cnty.geo <- geo.make(state="*", county="*")

# lookup function 
#acs.lookup(keyword = "median income", endyear = 2010, span=5, case.sensitive=F)

# population growth 2000 to 2010
pop2000 <- acs.fetch(endyear=2000, geography = cnty.geo, dataset="sf1", variable="P001001")
pop2010 <- acs.fetch(endyear=2010, geography = cnty.geo, dataset="sf1", variable="P0010001")

cnty.pop2000 <- data.frame(geography(pop2000), estimate(pop2000)) %>%
  mutate(cnty_fips = str_pad(paste0(state,county),5,pad="0")) %>%
  select(cnty_fips, pop2000=P001001)

cnty.pop2010 <- data.frame(geography(pop2010), estimate(pop2010)) %>%
  mutate(cnty_fips = str_pad(paste0(state,county),5,pad="0")) %>%
  select(cnty_fips, pop2010=P0010001)

popgrowth <- left_join(cnty.pop2000,cnty.pop2010, by="cnty_fips") %>%
  mutate(pgrowth = round((pop2010-pop2000)/pop2000,3)) %>%
  select(cnty_fips, pgrowth)

# acs variable names and codes
acs.vars <- data.frame(var.names=c("cnty_pop","num_houses","med_income","gini",
                                   "poverty","renter","plumbing","medyr_structure",
                                   "med_age","college","sfh_d","sfh_a","mob_home",
                                   "apt10","apt20","apt50","med_value"),
                       var.codes=c("B01003_001","B25001_001","B06011_001","B19083_001",
                                   "B17001_002","B25003_003","B25047_002","B25035_001",
                                   "B01002_001","B06009_004","B25024_002","B25024_003",
                                   "B25024_010","B25024_007","B25024_008","B25024_009",
                                   "B25077_001"),
                       stringsAsFactors = F)

# fetch acs data for counties
acs.data.raw <- acs.fetch(endyear=2010, span=5, cnty.geo, variable=acs.vars$var.codes,
                          col.names=acs.vars$var.names)

# Build data.frame of census data
acs.data <- data.frame(geography(acs.data.raw), estimate(acs.data.raw)) %>%
  mutate(cnty_fips = str_pad(paste0(state,county),5,pad="0")) %>%
  left_join(cnty.area, by="cnty_fips") %>%
  left_join(popgrowth, by="cnty_fips") %>%
  na.omit() %>%
  mutate(ppl_house=cnty_pop/num_houses,
         house_dens=num_houses/cnty_area,
         prent=renter/num_houses,
         med_age_struc=2010-medyr_structure,
         pcollege=college/cnty_pop,
         ppoverty=poverty/cnty_pop,
         psfh=(sfh_a + sfh_d)/num_houses,
         pmob_home=mob_home/num_houses,
         papt=(apt10+apt20+apt50)/num_houses) %>%
  select(cnty_fips,cnty_pop:papt) %>%
  set_rownames(NULL)

# Merge everything together ----
data.list <- list(wudata,
                  cnty_typology,
                  wyield,
                  tmax05_10,
                  precip05_10,
                  cnty_pvi,
                  acs.data)

data.full <- Reduce(function(x,y) 
  left_join(x,y,by="cnty_fips"), data.list) %>%
  left_join(climate_region, by="state.abb") %>%
  mutate(house_served = (pop_served/cnty_pop.x) * num_houses,
         wh = ((wthdrw_tw*1e6*365))/house_served) 

# select predictors
model.data <- data.full %>%
  select(cnty_fips,county,state,state.abb,climate_region, 
         rur2urb,econdep,retire,wh,cnty_pop=cnty_pop.y,
         pgrowth,prop_sw,Qmm,tmax_40,tmax_0510,tmax_diff,
         ppt_40,ppt_0510,ppt_diff,med_income,ppoverty,
         gini,med_age,pcollege,cnty.pvi,house_dens,
         ppl_house,med_value,med_age_struc,prent,
         psfh,pmob_home,papt) %>%
  na.omit() %>%
  filter(wh > 5e4 & wh < 3e5) %>%
  mutate_each(funs(as.numeric(scale(.))), -(cnty_fips:wh))

# look for high correlation
high.cor <- model.data %>%
  select(wh:papt) %>%
  correlate() %>%
  shave() %>%
  stretch() %>%
  na.omit() %>%
  filter(abs(r)>0.4)

# drop covariates based off correlations
model.data <- model.data %>%
  select(-c(tmax_0510,ppt_0510,ppoverty,
            med_value,pmob_home))













