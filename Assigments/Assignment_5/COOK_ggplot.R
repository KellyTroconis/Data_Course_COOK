library(tidyverse)
library(skimr)
library(plotly)
library(RColorBrewer)
library(colorspace)
library(colorblindr)

df <- iris
skim(df)


ggplot(df, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title ="Sepal length vs. Petal length
       for three iris species")+
  theme_minimal()

#png1
png(filename = "iris_fig1.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(df, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title ="Sepal length vs. Petal length", subtitle = "for three iris species") +
  theme_minimal()
dev.off()

#png2
png(filename = "iris_fig2.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(df, aes(x=Petal.Width, fill=Species)) +
  geom_density(alpha=.5) +
  labs(title ="Distribution of Petal Width", subtitle = "for three iris species", x= "Petal Width") +
  theme_minimal()
dev.off()

#png3
pw <- c("Sepal.Width","Petal.Width")

df2 <- subset(df,select = c("Petal.Width","Sepal.Width","Species"))
df2$Ratio <- (df2$Petal.Width / df2$Sepal.Width)

png(filename = "iris_fig3.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(df2,aes(x=Species,y=Ratio,fill=Species)) + 
  geom_boxplot() +
  labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species",
       y= "Ratio of Sepal Width to Petal Width") +
  theme_minimal()
dev.off()
 
#png4
#library you need
library(ggplot2)
theme_set(theme_classic())
#prep the data
data("iris")
iris$Ave <- rownames(iris) #creates a new culumn 
iris$Ave2 <- round((iris$Sepal.Length -  mean(iris$Sepal.Length)) /sd(iris$Sepal.Length),digits=2)#computed normalized length
iris$Ave_type <- ifelse(iris$Ave2 < 0, "below","above")#above / below avg flag
iris <- iris[order(iris$Ave2), ] #sort
iris$Ave <- factor(iris$Ave, levels = iris$Ave)#convert to factor to retain sorted order in plot.

ggplot(iris,aes(x=Ave,y=Ave2, fill=Species)) +
  geom_bar(stat = "identity", width=.8) +
  labs(title = "Sepal length Deviance from the Mean of all observations",
       caption = "Note: Deviance = Sepal.length - mean(Sepal.length)",
       y= "Deviance from the Mean",x="") +
  coord_flip() + theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


dp <- ggplot(iris,aes(x=Ave,y=Ave2, fill=Species)) +
  geom_bar(stat = "identity", width=.8) +
  labs(title = "Sepal length Deviance from the Mean of all observations",
       caption = "Note: Deviance = Sepal.length - mean(Sepal.length)",
       y= "Deviance from the Mean",x="") +
  coord_flip() + theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

png(filename = "iris_fig4.png", width = 6, height = 4, units = 'in', res = 300)
dp
dev.off()


