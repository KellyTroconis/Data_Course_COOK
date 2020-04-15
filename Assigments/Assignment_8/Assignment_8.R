library(modelr)
library(broom)
library(dplyr)
library(fitdistrplus)
library(tidyr)

##note:this is the clean(short) version. You can see the "dratf" in assigment_8_practice 
#1.  loads the "/Data/mushroom_growth.csv" data set

df <- read.csv("./../../Data_BIO3100/mushroom_growth.csv")

#2.  creates several plots exploring relationships between the response and predictors

df %>% ggpairs()
glimpse(df)

#3.  defines at least 2 models that explain the **dependent variable "GrowthRate"**
# + One must be a lm() and 
#+ one must be an aov()
mod1 <-  lm(GrowthRate ~ Light, data = df)
summary(mod1)
plot(df$GrowthRate ~ df$Light * df$Species) + abline(mod1, col="Blue")

mod2 <-  lm(GrowthRate ~ Nitrogen, data = df)
plot(df$GrowthRate ~ df$Nitrogen) + abline(mod2, col="Blue")

mod3 <- lm(GrowthRate ~ Humidity, data = df)
plot(df$GrowthRate ~ df$Humidity) + abline(mod3, col="Blue")

mod4 <-  aov(GrowthRate ~ Light, data = df)
plot(df$GrowthRate ~ df$Light) + abline(mod4, col="Blue")

mod5 <-  aov(GrowthRate ~ Nitrogen, data = df)
plot(df$GrowthRate ~ df$Nitrogen) + abline(mod5, col="Blue")

mod6 <- aov(GrowthRate ~ Humidity, data = df)
plot(mod6) + abline(mod6, col="Blue")

#4.  calculates the mean sq. error of each model
m1 <- mean(mod1$residuals^2)
m2 <- mean(mod2$residuals^2)
m3 <- mean(mod3$residuals^2)
m4 <- mean(mod4$residuals^2)
m5 <- mean(mod5$residuals^2)
m6 <- mean(mod6$residuals^2)

#5.  selects the best model you tried
### lm: mod1 

#6.  adds predictions based on new values for the independent variables used in your model
preds1 <-  add_predictions(df, mod1) 
ggplot(preds1, aes(x=Light, y=pred, color=Species)) +
  geom_point()

#preds2 <-  add_predictions(df, mod6) 
ggplot(preds1, aes(x=Humidity, y=pred, color=Species)) +
  geom_point()

#7.  plots these predictions alongside the real data
newdf <-  data.frame(Light = c(5,15,25))
predictions <-  predict(mod1, newdata = newdf)
{plot(df$GrowthRate ~ df$Light, xlim=c(0,25),ylim=c(50,200))
  points(x=newdf$Light, y=predictions, col="Red")
  abline(mod1)}

#ggplot(preds2, aes(x=GrowthRate, y=pred)) +
  #geom_point()
  
#1.  Are any of your predicted response values from your best model scientifically meaningless? Explain.
#a small p-value means that we can reject the null hypothesis.There's a relationship between Growthrate and Light (Pr(>t)= 1.65e-12 ***). This makes it not meaningless. 

#2.  In your plots, did you find any non-linear relationships?  If so, do a bit of research online and give a link to at least one resource explaining how to deal with this in R
# GrowthRate ~ Humidity is not linear. I am still working on undertanding anova. https://www.guru99.com/r-anova-tutorial.html
