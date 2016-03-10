theme_blank <- function(text.size=18){
  require(grid)
  theme(panel.background = element_rect(fill = "transparent",color=NA),
        plot.background = element_rect(fill = "transparent",color=NA),
        legend.background = element_rect(fill = "transparent",color=NA),
        text = element_text(size=text.size),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,-1,-1),"lines"))
}