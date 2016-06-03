

# calculate the median and mean of the Wn
summarise(group_by(wudata, census_region), mn = mean(wn), md = median(wn))

# standardized coefficients ---> metric coefficients
# https://www3.nd.edu/~rwilliam/stats1/x92.pdf
# beta1 = beta1' * std(y)/std(x1)

## select mean posterior estimate of the largest abs(coefficient) for each region and calculate
## metric coefficients
max.coeff <- summarise(group_by(g.cens.regions2, cens.regions), value = max(abs(value.x))) %>% 
  mutate(parameter=0, sdy=0, sdx=0)

for (i in 1:4){
  idx <- which(abs(g.cens.regions2$value.x)==max.coeff$value[i])
  max.coeff$parameter[i] <- g.cens.regions2$Parameter[idx]
  
  ### vector of logwn for region
  hold1 <- wudata %>% filter(census_region==max.coeff$cens.regions[i]) %>% 
    dplyr::select(logwn) %>% filter(logwn!=min(logwn))
  
  max.coeff$sdy[i] <- sd(hold1[,1])
  
  ### vector of parameter for region
  hold2 <- wudata %>% filter(census_region==max.coeff$cens.regions[i]) %>%
    dplyr::select(match(max.coeff$parameter[i],names(wudata)))
  
  max.coeff$sdx[i] <- sd(hold2[,1])
} 
max.coeff$beta2 <- max.coeff$value * (max.coeff$sdy/max.coeff$sdx)

# percent change in response for 1 unit change in predictor
#http://www.kenbenoit.net/courses/ME104/logmodels2.pdf
max.coeff$percent_delta <- (10^(max.coeff$beta2) - 1) 

