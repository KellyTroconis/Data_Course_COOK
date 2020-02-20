library(tidyverse)
library(skimr)
library(plotly)
library(RColorBrewer)
library(colorspace)
library(colorblindr)
library(patchwork)

devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")
library(colorspace)
library(colorblindr)
library(ggimage)

palette_plot(brewer.pal())


cvd_grid(p1)
RColorBrewer::brewer.pal.info
cvd_grid(palette_plot(pal))

df <- mtcars

skim(df)


p1 <- ggplot(df, aes(x=disp, y=mpg, color=factor(cyl))) +
  geom_point()
#ggsave("./Desktop/Data_Course_COOK/testplot.png", dpi=600, height = 4, width = 4)#it will save the plot above it by defult 

p2 <- ggplot(df, aes(x=disp, y=mpg, color=gear)) +
  geom_smooth()
p1
ggsave(filename = "./testplot.png", plot = p1, device="png")
p2
ggsave(filename = "./testplot2.png", plot = p2, device="png")

p3 <- ggplot(df, aes(x=hp, y=mpg, color=gear)) +
  geom_smooth()

p1/p2
p1+p2
(p1+p2)/p3
ggsave("multiplot.png")
getwd()

pal = c("#c4a113","#c1593c","#643d91","#820616","#477887","#688e52",
        "#12aa91","#705f36","#8997b2","#753c2b","#3c3e44","#b3bf2d",
        "#82b2a4","#894e7d","#a17fc1","#262a8e","#abb5b5","#000000")
palette_plot(pal)
p1 <- ggplot(df, aes(x=disp, y=mpg, color=factor(cyl), size=3)) +
  geom_point() 

p5 <- p1 + theme_bw() +
  labs(title = "Miles per Gallon vs. Displacement", x= "Engine displacement", y= "miles per gallon", color= "number of\ncylinders") +
  scale_color_manual(values=pal)

p4 <- p1 + scale_x_reverse()


p5 + theme(title =element_text(color = "Blue", face="italic"),
           panel.background = element_rect(fill = "Blue"), 
           plot.background = element_rect(fill= "Red"),
           legend.background = element_rect(fill = "Purple"),
           panel.grid = element_line(size =2),
           legend.text = element_text(size = 20),
           axis.text.y = element_text(angle = 20),
           axis.title.x = element_text(angle = 180))


mod <- lm(data=df, formula = mpg ~ disp ) #linean regreation model calculates the best fit line 
summary(mod)
residuals(mod)
df$resids <- residuals(mod)

df <- mutate(df, DIFF=mpg-resids) #data transformation 


ggplot(df, aes(x=disp, y=mpg)) +
  geom_point() + geom_smooth(method = "lm", se=FALSE) +
  geom_segment(aes(yend=DIFF, xend=disp))

ggplot(df, aes(x=disp, y=mpg)) +
  geom_point() + geom_smooth(method = "lm", se=FALSE) +
  scale_x_log10()

ggplot(df, aes(x=sqrt(disp), y=mpg)) +
  geom_point() + geom_smooth(method = "lm", se=FALSE) 

mf <- read.csv("./../Data_Course/Data/mushroom_growth.csv")
glimpse(mf)


#when geom bar does not work use geom bar 
ggplot(mf, aes(x=Light, y=GrowthRate, color= Species)) +
  geom_smooth() +
  theme_minimal()

ggplot(mf, aes(x=Nitrogen, y=GrowthRate, color=Light)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x,2)) +
  facet_wrap(~Species) +
  scale_color_gradient(low="Blue", high="Red") +
  theme(strip.text = element_text(face = "italic"))

mod2 <- lm(data = mf, GrowthRate ~ poly(Nitrogen,2))
summary(mod2)

ggimage::geom_image()

df$fn <- "./car.png"
p <- ggplot(df, aes(x=sqrt(disp), y=mpg)) +
  geom_image(aes(image=fn)) + geom_smooth(method = "lm", se=FALSE) +
  theme_minimal() +
  


library(gganimate)

p + transition_manual(mpg) 



