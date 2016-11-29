
#load libraries
library(pacman)
p_load(dplyr,ggplot2,coefplot,rstan,arm,ggmcmc,coda, rethinking)

# set working directory
setwd("~/Water Conservation/R_conservation/USGSwaterUse/usgs-wu-mlm/Fall_2016")

# select only numeric covariates
mlm.data <-  model.data %>%
  mutate(econdep=factor(econdep, levels=unique(econdep)))

# custom functions
grab.num <- function(x){as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))}
select <- dplyr::select
rename <- dplyr::rename

# Bayesian multi level linear model fit in Stan
group <- as.integer(mlm.data$econdep)
X <- model.matrix(~.,select(mlm.data,cnty_pop:papt))
X <- X[,order(colnames(X))]

# list of data for stan
stan_data <- list(y = mlm.data$wh,
                  group = group,
                  X = X,
                  K = ncol(X),
                  N = nrow(X),
                  J = length(unique(group)),
                  nu = 2)


# fit model
stan.model <- stan_model('scripts/stan/mlm_normal.stan')
stan.fit2 <- sampling(stan.model, stan_data, iter = 1000, chains = 2)

# find link between levels and names
levels.link <- data.frame(names = levels(mlm.data$econdep),
                          levels = 1:nlevels(mlm.data$econdep))

# extract fit
s2 <- ggs(stan.fit2, family="beta") %>% 
  mutate(num = grab.num(Parameter)) %>%
  mutate(levels = as.numeric(substr(as.character(num),1,1))) %>%
  mutate(coef.index = as.numeric(substring(as.character(num), 2))) %>%
  inner_join(levels.link, by = "levels") %>%
  mutate(Parameter =  colnames(X)[coef.index]) %>%
  group_by(names,Parameter) %>%
  summarize(low = HPDI(value,0.67)[1],
            high = HPDI(value,0.67)[2],
            lower = HPDI(value,0.89)[1],
            higher = HPDI(value,0.89)[2],
            mode = chainmode(value)) %>%
  mutate(Parameter=factor(Parameter, 
                          levels=Parameter[order(mode)], 
                          ordered=TRUE)) 

# new plot
ggplot(filter(s2, Parameter !="(Intercept)")) + theme_bw() +
  geom_pointrange(aes(x=Parameter,y=mode,ymin=low,ymax=high),  
                  shape=21, position=position_dodge(width=0.6)) + 
  facet_wrap(~names) + coord_flip() +
  #theme(panel.border = element_rect(colour = "grey20", fill=NA, size=1)) +
  geom_hline(yintercept=0, color="black") +
  ylab(expression(post. ~ distribution ~ of ~ beta ~ parameters)) + xlab(NULL)

ggplot(filter(s, Parameter !="(Intercept)")) + theme_bw() +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange(aes(x=Parameter,y=mode,ymin=low,ymax=high), shape=21,
                  fill="white") + coord_flip() +
  geom_point(aes(Parameter,lower),shape="+", size=2) + 
  geom_point(aes(Parameter,higher),shape="+", size=2) +
  labs(y="67% and 89% HPDI with MAP point estimate") +
  ggtitle("Fully pooled parameter estimates, Laplace prior") +
  labs(subtitle="–o– = 67% HPDI,    + –o– + = 89% HPDI")

