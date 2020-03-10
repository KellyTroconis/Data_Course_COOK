#Stepwise AIC 

library(MASS)
library(tidyverse)
library(modelr)

data("mtcars")

fullmodel <- lm(data= mtcars, mpg ~ factor(cyl) + disp + hp + wt + drat + qsec + factor(vs) +
                  factor(am) + factor(gear) + factor(carb))#......

step <- stepAIC(fullmodel) #automated model simple 
step$call #gives the simpels one 
goodmodel <- lm(formula= mpg ~ factor(cyl) + hp + wt + factor(am),data = mtcars)#....
mtcars %>% add_predictions(goodmodel)

set.seed(123)
set <- caret::createDataPartition(mtcars$mpg)
set <- set$Resample1

train <- mtcars[set,]
test <- mtcars[-set,]

formula(goodmodel)
trainedmodel <- lm(data = train, formula= formula(goodmodel))

add_predictions(test, trainedmodel) %>%
  ggplot(aes(x=hp, color= factor(cyl))) +
  geom_point(aes(y=mpg)) + geom_smooth(metod= "lm", aes(y=pred))




