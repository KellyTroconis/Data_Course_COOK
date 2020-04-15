library(modelr)
library(broom)
library(dplyr)
library(fitdistrplus)
library(tidyr)
library(tidyverse)
library(GGally)
library(lindia) #diagnostic for plots lm gg_diagnose
library(skimr)
library(patchwork) #add plots together
library(caret) #to train models 
(.packages())
getwd()

#1.  loads the "/Data/mushroom_growth.csv" data set

df <- read.csv("./../../Data_BIO3100/mushroom_growth.csv")

#2.  creates several plots exploring relationships between the response and predictors

df %>% ggpairs()
glimpse(df)

df$Light <- as.numeric(df$Light)
df$Nitrogen <- as.factor(df$Nitrogen)
df$Temperature <- as.factor(df$Nitrogen)
#3.  defines at least 2 models that explain the **dependent variable "GrowthRate"**
# + One must be a lm() and 
#+ one must be an aov()

modA <-  lm(GrowthRate ~ Light * Humidity * Nitrogen * Temperature * Species, data = df)
summary(modA)
plot(df$GrowthRate ~ df$Light * df$Humidity * df$Nitrogen * df$Temperature * df$Species) + abline(modA, col="Blue")


modB <-  lm(GrowthRate ~ Light * Nitrogen * Species, data = df)
summary(modB)
plot(df$GrowthRate ~ df$Light * df$Nitrogen * df$Species) + abline(modB, col="Blue")

modC <-  lm(GrowthRate ~ Nitrogen, data = df)
summary(modC)
plot(df$GrowthRate ~ df$Nitrogen) + abline(modC, col="Blue")

modD <-  lm(GrowthRate ~ Temperature, data = df)
summary(modD)
plot(df$GrowthRate ~ df$Temperature) + abline(modD, col="Blue")


mod1 <- aov(GrowthRate ~ Humidity * Light * Species, data = df)
mod2 <- aov(GrowthRate ~ Nitrogen, data = df)
mod3 <- aov(GrowthRate ~ Light * Temperature * Species, data = df)
mod4 <- aov(GrowthRate ~ Temperature, data = df)
mod5 <- aov(GrowthRate ~ Light * Humidity * Nitrogen * Temperature * Species, data = df)
#4.  calculates the mean sq. error of each model

mA <- mean(modA$residuals^2)
mB <- mean(modB$residuals^2)
mC <- mean(modC$residuals^2)
mD <- mean(modD$residuals^2)
m1 <- mean(mod1$residuals^2)
m2 <- mean(mod2$residuals^2)
m3 <- mean(mod3$residuals^2)
m4 <- mean(mod4$residuals^2)
m5 <- mean(mod5$residuals^2)

#5.  selects the best model you tried
# from lm: modA, modB. from aov: mod1, mod5

#6.  adds predictions based on new values for the independent variables used in your model
predsA <-  add_predictions(df, modA) 
ggplot(predsA, aes(x=Light, y=pred, color=Species)) +
  geom_point()

predsB <-  add_predictions(df, modB) 
ggplot(predsB, aes(x=Light, y=pred, color=Species)) +
  geom_point()

preds1 <-  add_predictions(df, mod1) 
ggplot(preds1, aes(x=Light, y=pred, color=Species)) +
  geom_point()

preds5 <-  add_predictions(df, mod5) 
ggplot(preds5, aes(x=Light, y=pred, color=Species)) +
  geom_point()

#7.  plots these predictions alongside the real data
# Make a new dataframe with the predictor values we want to assess
# mod1 only has "disp" as a predictor so that's what we want to add here

newdf <-  data.frame(Light = c(25,30,35), Humidity= 'High', Nitrogen= '20', Temperature= '20', Species= 'P.ostreotus')
predictions <-  predict(modA, newdata = newdf)


{plot(df$GrowthRate ~ df$Light * df$Humidity * df$Nitrogen * 
       df$Temperature * df$Species, xlim=c(0,60),ylim=c(-5,220)) +
  points(x=newdf$Light * newdf$Humidity * newdf$Nitrogen * newdf$Temperature * newdf$Species, y=predictions, col="Red")
  abline(modA, col="Blue")}



# plot those predictions on our original graph
{plot(mtcars$mpg ~ mtcars$disp,xlim=c(0,1000),ylim=c(-10,50))
  points(x=newdf$disp,y=predictions, col="Red")
  abline(mod1)}

p1 <- ggplot(df_mod1,aes(x=Sepal.Width,color=Species)) +
  geom_point(aes(y=Sepal.Length),alpha=.5,size=2) +
  geom_point(aes(y=pred),color="Black") + theme_bw()

ggplot(aes(x=wt,color=factor(cyl))) +
  geom_point(aes(y=mpg)) +
  geom_smooth(method = "lm",aes(y=pred)) +
  labs(title = "wt + cyl")



