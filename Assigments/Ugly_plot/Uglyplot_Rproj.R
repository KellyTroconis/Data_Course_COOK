library(tidyverse)
library(skimr)
library(plotly)

library(ggimage)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)

df <- data("iris")
glimpse(iris)
skim(iris)
names(iris)


iris$fn <- "./../../bee.png"

p1 <- ggplot(iris, aes(y=Sepal.Length,x=Petal.Length,label=Species)) +
  geom_point(size = 20) +
  geom_image(aes(image=be)) +
  geom_text(aes(Petal.Length, Sepal.Length, label= Species), nudge_x = 1.5) +
  geom_text(aes(x = 3, y = 7.5, label = "I SEE YOU", color= "Pink")) +
  geom_text_repel() +
  labs(title = "..............A.............B...............C", x= "length", y="length") +
  scale_color_brewer(palette = "RdBu")


    
  
p2 <- p1 + theme(title =element_text(color = "Black", face="italic"),
           legend.background = element_rect(fill ="Black"),
           legend.position = c(0.3, 0.6),
           panel.grid = element_line(size =.001),
           legend.text = element_text(size =.001),
           axis.text.y = element_text(angle = 145),
           axis.title.y = element_text(angle = 180),
           axis.title.x = element_text(angle = 90),
           panel.background = element_rect(fill = "#F4A582"), 
           plot.background = element_rect(fill= "#92C5DE"),
           axis.text.x = element_text(angle = 145))
p2
ggsave(filename = "./uglyplot.png", plot = p2, device="png")

p3 <- p2 + theme(panel.background = el )

brewer.pal(n = 11, name = 'RdBu')
pal <- c("#D6604D", "#F4A582", "#FDDBC7")
p2
p2 + theme()


  