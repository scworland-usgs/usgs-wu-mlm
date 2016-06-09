
stan.regression <- function(df, stan_model_file, stan_data, 
                            stan_params=NULL, iter=1000, chains=1, 
                            group.name=NULL) {
  
  require(rstan); require(ggmcmc)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  # If a multilevel model, run this section
  if ("group" %in% names(stan_data)) {
    
    stan.model <- stan_model(stan_model_file)
    stan.fit <- sampling(stan.model, stan_data, stan_params = stan_params, iter = iter, chains = chains)
    
    g.i <- match(group.name,names(df)) # column index of groups
    
    # find link between levels and names
    levels.link <- data.frame(names = levels(df[,g.i]),
                              levels = 1:nlevels(df[,g.i]))
    
    results.long <- ggs(stan.fit, family="beta") %>% 
      mutate(num = as.numeric(gsub("[^\\d]+", "", Parameter, perl=TRUE))) %>%
      mutate(levels = as.numeric(substr(as.character(num),1,1))) %>%
      mutate(coef.index = as.numeric(substring(as.character(num), 2))) %>%
      inner_join(levels.link, by = "levels") %>%
      mutate(variable =  colnames(X)[coef.index])
    
    
    results.short <- results.long %>% group_by(variable, names) %>%
      summarise(p97.5=quantile(value, probs=0.975),
                p2.5=quantile(value, probs=0.025),
                p50=quantile(value, probs=0.5))
    
    return(list(stan.fit,results.long,results.short))
    
  } else {
  
    # if no groups (pooled model), run this section
    stan.model <- stan_model(stan_model_file)
    stan.fit <- sampling(stan.model, stan_data, stan_params = stan_params, iter = iter, chains = chains)
    
    results.long <- ggs(stan.fit, family="beta") %>%
      mutate(num = as.numeric(gsub("[^\\d]+", "", Parameter, perl=TRUE))) %>%
      mutate(variable =  colnames(X)[num]) 
  
    results.short <- results.long %>% group_by(variable) %>%
      summarise(p97.5=quantile(value, probs=0.975),
                p2.5=quantile(value, probs=0.025),
                p50=quantile(value, probs=0.5)) 
    
    return(list(stan.fit,results.long,results.short))
    
  }
}
  