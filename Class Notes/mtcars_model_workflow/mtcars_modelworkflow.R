library(tidyverse)
library(modelr)
library(GGally)
library(lindia)
library(skimr)
library(patchwork)
library(caret)

data("mtcars")
mtcars %>% ggpairs()
skim(mtcars)
#modify data first 
mtcars$am[mtcars$am==0]<-"Automatic"
mtcars$am[mtcars$am==1]<-"Manual"
mtcars$am<-as.factor(mtcars$am)
mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$vs<-as.factor(mtcars$vs)
mtcars$gear<-as.factor(mtcars$gear)
mtcars$carb<-as.factor(mtcars$carb)
str(mtcars)
#look at the data again
mtcars %>% ggpairs()
mtcars %>% filter(am == "Automatic") %>% ggpairs()
mtcars %>% ggpairs(mapping = c("am","mpg"))
mtcars %>% ggpairs(mapping = c("cyl","mpg"))
mtcars %>% ggpairs(mapping = c("carb","mpg"))
# lm() is linear model. There are LOTS of other model types
mod1 <- lm(data=mtcars, formula = mpg ~ cyl)
mod2 <- lm(data=mtcars, formula = mpg ~ cyl + am)
mod3 <- lm(data=mtcars, formula = mpg ~ cyl * am)
mod4 <- lm(data=mtcars, formula = mpg ~ cyl + carb)
mod5 <- lm(data=mtcars, formula = mpg ~ cyl * carb)
mod6 <- lm(data=mtcars, formula = mpg ~ am)
mod6_1 <- lm(data=mtcars, formula = mpg ~ am + cyl)
mod6_2 <- lm(data=mtcars, formula = mpg ~ am * cyl)
mod7 <- lm(data=mtcars, formula = mpg ~ gear)
mod8 <- lm(data=mtcars, formula = mpg ~ gear + vs)
mod9 <- lm(data=mtcars, formula = mpg ~ gear * vs)
moddrat <- lm(data=mtcars, formula = mpg ~ drat)
moddrat1 <- lm(data=mtcars, formula = mpg ~ drat + cyl)
moddrat2 <- lm(data=mtcars, formula = mpg ~ drat * cyl)


# Look at model summaries
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod8)
summary(moddrat)
gg_diagnose(mod1)
gg_diagnose(moddrat)
gg_diagnose(mod7)

# Compare models ####
anova(mod1, mod2) # different?
anova(mod1, mod3)
anova(mod2, mod3)
anova(mod1, mod4)
anova(mod1, mod5)
anova(mod5, mod4)
anova(mod1, mod6)
anova(mod7, mod8)
anova(moddrat, moddrat1) # different?
anova(moddrat, moddrat2)
anova(moddrat1, moddrat2)
# which has better fit ?
mod1mse <- mean(residuals(mod1)^2)
mod2mse <- mean(residuals(mod2)^2)
mod3mse <- mean(residuals(mod3)^2)
mod6mse <- mean(residuals(mod6)^2)
mod4mse <- mean(residuals(mod4)^2)
mod5mse <- mean(residuals(mod5)^2)
mod7mse <- mean(residuals(mod7)^2)
mod8mse <- mean(residuals(mod8)^2)
moddratmse <- mean(residuals(moddrat)^2)
moddrat1mse <- mean(residuals(moddrat1)^2)
moddrat2mse <- mean(residuals(moddrat2)^2)
mod1mse ; mod2mse ; mod3mse ; mod4mse ; mod5mse ; mod6mse ; mod7mse ; mod8mse#the smallest number is the best model 
# Evaluate predictions ####
df_mod3 <- add_predictions(mtcars,mod3) # adds model prediction column using a single model
df_mod3
df <- gather_predictions(mtcars, mod1,mod2,mod3) # add many models' predictions at once (tidy-style)
df
skim(df)
names(df)
df_mod6 <- gather_predictions(mtcars, mod6,mod6_1,mod6_2)
df_mod7 <- gather_predictions(mtcars, mod7,mod8,mod9)
df_moddrat <- gather_predictions(mtcars, moddrat,moddrat1,moddrat2)
# compare predictions to reality
p1 <- ggplot(df_mod3,aes(x=cyl,color=am)) +
  geom_point(aes(y=mpg),alpha=.5,size=2) +
  geom_point(aes(y=pred),color="Black") + theme_bw()
p1

p1 + geom_segment(aes(y=mpg,xend=wt,yend=pred),
                  linetype=2,color="Black",alpha=.5)

# look at plot of predictions for each model
# we can do this after using gather_predictions() with more than 1 model
ggplot(df,aes(x=cyl,color=am)) +
  geom_point(aes(y=mpg),alpha=.5,size=2) +
  geom_point(aes(y=pred),color="Black") + 
  facet_wrap(~model) +
  theme_bw()

ggplot(df_mod6,aes(x=am,color=cyl)) +
  geom_point(aes(y=mpg),alpha=.5,size=2) +
  geom_point(aes(y=pred),color="Black") + 
  facet_wrap(~model) +
  theme_bw()

ggplot(df_mod7,aes(x=gear,color=vs)) +
  geom_point(aes(y=mpg),alpha=.5,size=2) +
  geom_smooth(method= "lm", aes(y=pred),color="Black") + 
  facet_wrap(~model) +
  theme_bw()

ggplot(df_moddrat, aes(x=drat,color= cyl)) +
  geom_point(aes(y=mpg)) +
  geom_smooth(method = "lm", aes(y=pred)) +
  facet_wrap(~model) +
  theme_bw()
set.seed(123) # set reproducible random number seed
set <- caret::createDataPartition(mtcars$drat, p=.5) # pick random subset of data 
set <- set$Resample1
train <- mtcars[set,] # subset iris using the random row numbers we made
test <- mtcars[-set,] # The other half of the iris dataset

# build our best iris model (mod3, from above)
formula(moddrat2)
moddrat2_cv <- lm(data=train, formula = formula(moddrat2))


# Test trained model on unused other half of data set
mtcarstest <- add_predictions(test,moddrat2_cv)

# plot it
ggplot(mtcarstest,aes(x=drat,color=cyl)) +
  geom_point(aes(y=mpg),alpha=.25) +
  geom_point(aes(y=pred),shape=5)

# compare MSE from our over-fitted model to the cross-validated one
testedresiduals <- (mtcarstest$pred - mtcarstest$mpg)

moddrat2mse # our original MSE
mean(testedresiduals^2) # our cross-validated model... comparing this the model is too good to be use 

# Plot comparison of original and validated model 

# gather model predictions
dfdcv <- gather_predictions(mtcars, moddrat2,moddrat2_cv)

# plot - distinguish model predictions using "linetype"
ggplot(dfdcv, aes(x=drat,color=cyl)) +
  geom_point(aes(y=mpg),alpha=.2) +
  geom_smooth(method = "lm",aes(linetype=model,y=pred)) + 
  theme_bw()

