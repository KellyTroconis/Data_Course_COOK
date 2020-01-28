library(tidyverse)

#load some data
data("iris")

# subset

iris <- iris %>% filter(Species=="virginica")

glimpse(iris)
plot(y=iris$Sepal.Length,x=iris$Petal.Length,col=iris$Species,pch=20, main = "NOOO", xlab ="petal length", ylab = "sepal length")

summary(iris)

plot(x=iris$Species,y=iris$Sepal.Length,pch=20, main = "NOOO", xlab ="petal length", ylab = "sepal length")


hist(iris$Sepal.Length, breaks = 20)
plot(density(iris$Sepal.Length))

# How to save a plot
jpeg("densityplot.jpeg")
plot(density(iris$Sepal.Length))
dev.off()