#ggplot theme and scale code

library(RColorBrewer)
theme1<- theme(plot.title = element_text(face = "bold", hjust=0.5)) + theme(
  panel.background = element_rect(fill = "white",
                                  colour = "white",
                                  size = 0.20, linetype = "solid"),
  panel.grid.major = element_line(size = 0.20, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.20, linetype = 'solid',
                                  colour = "gray"))

color1<- scale_fill_brewer(palette="Set2")


