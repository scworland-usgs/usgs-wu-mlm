


density.hist <- function(df, x=NULL, y=NULL) {
  
require(ggplot2)
require(gridExtra)
require(devtools)
  
htop <- ggplot(data=df, aes_string(x=x)) + 
  geom_histogram(aes(y=..density..), fill = "white", color = "black", bins=100) + 
  stat_density(colour = "blue", geom="line", size = 1, position="identity", show.legend=FALSE) +
  theme_bw() + theme(axis.title.x = element_blank())

blank <- ggplot() + geom_point(aes(1,1), colour="white") +
  theme(axis.ticks=element_blank(), panel.background=element_blank(), panel.grid=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank())

scatter <- ggplot(data=df, aes_string(x=x, y=y)) + 
  geom_point(size = 0.6) + stat_ellipse(type = "norm", linetype = 2, color="green",size=1) +
  stat_ellipse(type = "t",color="green",size=1) +
  theme_bw() + labs(x=x, y=y)

hright <- ggplot(data=df, aes_string(x=x)) + 
  geom_histogram(aes(y=..density..), fill = "white", color = "black", bins=100) + 
  stat_density(colour = "red", geom="line", size = 1, position="identity", show.legend=FALSE) +
  coord_flip() + theme_bw() + theme(axis.title.y = element_blank())

grid.arrange(htop, blank, scatter, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

}


# example
# density.hist(df, x = "xdata", y = "ydata")


