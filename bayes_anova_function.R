

bayes.anova <- function(df, y, group, iter=1000, chains=4) {
  require(rstan)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  # indices
  y.i <- match(y,names(df)) # column index of response variable
  g.i <- match(group,names(df)) # column index of groups
  
  # stan list
  anova.list <- list(N = nrow(df), # number of observations
                     y = df[,y.i], # column of response variable
                     group = df[,g.i], # column of groups
                     J=nlevels(factor(df[,g.i]))) # number of group level


  # fit model
  anova.fit <- stan(file='anova_stan.stan', data=anova.list,
                    iter=iter, chains=chains)
  
  # extract values
  anova.df <- summary(anova.fit, c("s_y", "s_a"))$summary

  anova.df <- data.frame(anova.df,
                         Source = factor(rownames(anova.df),
                                         levels = rownames(anova.df),
                                         labels = c("error", group)),
                         df = with(anova.list, c(N-J, J-1)))
  
  return(anova.df)
}











