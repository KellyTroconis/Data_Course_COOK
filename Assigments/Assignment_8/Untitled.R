library(modelr)
library(broom)
library(dplyr)
library(fitdistrplus)
library(tidyr)
library(tidyverse)
library(GGally)
library(lindia)
library(skimr)
library(patchwork)
library(caret)


df <- read.csv("./../Data/mushroom_growth.csv")

df %>% ggpairs()
glimpse(df)

modA = lm(GrowthRate ~ Nitrogen * Species * Light * Humidity * Temperature, data = df)
summary(modA)
plot(df$GrowthRate ~ df$Nitrogen + df$Light) + abline(modA)

modB = lm(GrowthRate ~ Light, data = df)
summary(modB)
plot(df$GrowthRate ~ df$Light) + abline(modB, col="Blue")

modC = lm(GrowthRate ~ Nitrogen * Humidity, data = df)
summary(modC)
plot(df$GrowthRate ~ df$Humidity) + abline(modC, col="Blue")

modD = lm(GrowthRate ~ Nitrogen * Temperature, data = df)
summary(modD)
plot(df$GrowthRate ~ df$Temperature) + abline(modD, col="Blue")


mean(modA$residuals^2)
mean(modB$residuals^2)
mean(modC$residuals^2)
mean(modD$residuals^2)
stepAIC(modA, direction="both")
