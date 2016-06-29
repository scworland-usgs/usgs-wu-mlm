
library(dplyr); library(lme4); library(ggplot2)

# load data (see mlm_data_prep.R)
load("model.data.Rda")

# useful values
nrows <- nrow(model.data)
ncols <- ncol(model.data)

# fraction of data to test models
set.seed(1)
model.data <- sample_frac(model.data,1)

# completely pooled ----
lm.formula <- reformulate(colnames(model.data[,4:ncols]),
                          response='wn')

lm.pooled <- lm(lm.formula,data=model.data)$coef %>% 
  data.frame() %>%
  add_rownames() %>%
  setNames(c("variable","value.pooled"))

# lmer urban class ----
region = "CDC_urban"
lmer.formula.urban <- reformulate(paste0("(1 + ",colnames(model.data[,4:ncols]), "|",
                                         region,")",collapse="+"), response = 'wn')

## fit model
lmer.fit.urban <- lmer(lmer.formula.urban, data = model.data)

cf.lmer.urban = coef(lmer.fit.urban)$CDC_urban %>%
  mutate(names = levels(wudata$CDC_urban)) %>% 
  melt() %>% rename(value.lmer=value)
  

## plot coeff for urban class
ggplot(cf.lmer.urban) + 
  geom_pointrange(aes(short.names,p50, ymin=p2.5, ymax=p97.5)) +
  geom_point(aes(short.names,value),color="red") + 
  geom_hline(yintercept=0, color="black") + theme_bw(base_size=10) + 
  facet_wrap(~names, ncol=2) + coord_flip() + 
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) + xlab(NULL)

# lmer census region ----
region = "census_region"
lmer.formula.census <- reformulate(paste0("(1 + ",colnames(model.data[,4:ncols]), "|",
                                         region,")",collapse="+"), response = 'wn')



## fit model
lmer.fit.census <- lmer(lmer.formula.census, data = model.data)

cf.lmer.census = coef(lmer.fit.census)$census_region %>%
  mutate(names = levels(wudata$census_region)) %>% 
  melt() %>% rename(value.lmer=value) 


## plot coeff for census region
ggplot(cf.lmer.census) + 
  geom_pointrange(aes(short.names,p50, ymin=p2.5, ymax=p97.5)) +
  geom_point(aes(short.names,value),color="red") + 
  geom_hline(yintercept=0, color="black") + theme_bw(base_size=10) + 
  facet_wrap(~names, ncol=2) + coord_flip() + 
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) + xlab(NULL)

# No pooling models ----

## urban class
urban.list <- vector("list", nlevels(wudata$CDC_urban))
for (i in 1:nlevels(wudata$CDC_urban)){
level <- i
name <- head(wudata$CDC_urban[model.data$CDC_urban==level],1)
hold <- model.data %>% filter(CDC_urban == level)

lm <- coef(lm(reformulate(colnames(hold[,4:ncols]),
                          response='wn'), data=hold)) %>% 
  data.frame() %>% 
  add_rownames() %>% 
  setNames(c("variable","value.lm")) %>%
  mutate(names = name) 

urban.list[[i]] = lm
}

urban.lm.nopooled <- do.call(rbind.data.frame, urban.list)

## census region
census.list <- vector("list", nlevels(wudata$census_region))
for (i in 1:nlevels(wudata$census_region)){
  level <- i
  name <- head(wudata$census_region[model.data$census_region==level],1)
  hold <- model.data %>% filter(census_region == level)
  
  lm <- coef(lm(reformulate(colnames(hold[,4:ncols]),
                            response='wn'), data=hold)) %>%
    data.frame() %>% 
    add_rownames() %>% 
    setNames(c("variable","value.lm")) %>%
    mutate(names = name) 
  
  census.list[[i]] = lm
}

census.lm.nopooled <- do.call(rbind.data.frame, census.list)

# combine frequentist methods ----
## urban class
urban_freq <- urban.lm.nopooled %>%
  inner_join(cf.lmer.urban, by = c("variable","names")) %>%
  inner_join(lm.pooled, by="variable") %>%
  inner_join(short.names, by ="variable") %>%
  select(names, short.names, value.pooled, value.lm, value.lmer)

urban_freqm <- urban_freq %>%
  melt(id.vars=c("short.names","names"))
colnames(urban_freqm)[3] <- "type"

ggplot(urban_freqm) + 
  geom_errorbar(data=urban.summary,(aes(short.names, ymin=p2.5, ymax=p97.5)), 
                size=0.5, alpha=1, width=0.5) +
  geom_point(aes(short.names,value, fill=type), shape=21, size=2, alpha=0.5) +
  facet_wrap(~names) + coord_flip() + theme_bw() + 
  geom_hline(aes(yintercept=0)) + 
  scale_fill_manual(values = c('blue','gold','green'),
                    labels=(c("pooled","no-pooling","partial-pooling"))) +
  ylab(expression(beta ~ parameter ~ estimates)) + xlab(NULL)

## census_regions
census_freq <- census.lm.nopooled %>%
  inner_join(cf.lmer.census, by = c("variable","names")) %>%
  inner_join(lm.pooled, by="variable") %>%
  inner_join(short.names, by ="variable") %>%
  select(names, short.names, value.pooled, value.lm, value.lmer)

census_freqm <- census_freq %>%
  melt(id.vars=c("short.names","names"))
colnames(census_freqm)[3] <- "type"

ggplot(census_freqm) + 
  geom_errorbar(data=census.summary,(aes(short.names, ymin=p2.5, ymax=p97.5)), 
                 size=0.5, alpha=1, width=0.5) +
  geom_point(aes(short.names,value, fill=type), shape=21, size=2, alpha=0.5) +
  facet_wrap(~names) + coord_flip() + theme_bw() + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_fill_manual(values = c('blue','gold','green'),
                    labels=(c("pooled","no-pooling","partial-pooling"))) +
  ylab(expression(beta ~ parameter ~ estimates)) + xlab(NULL)



