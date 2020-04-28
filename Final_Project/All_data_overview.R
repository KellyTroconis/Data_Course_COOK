library(readxl)
library(tidyverse)
library(modelr)
library(GGally)
library(skimr)
library(corrplot)
library(gmodels)

df <- read.csv("./Police_clean.cvs")
summary(df)
skim(df)
glimpse(df)
prop.table(table(df$Position))
#79.2% are swom, #20.8% disp

df <- na.omit(df)
df$Position <- factor(df$Position) 

levels(df$Position) <- c("SwomO", "Disp")
df$how.many.years <- factor(df$how.many.years)
levels(df$how.many.years) <- c("1-6", "7-13", "14-20", "20+")

#distribution
CrossTable(df$how.many.years,df$Position,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")

ggplot(df, aes(x= how.many.years)) +
  geom_bar() +
  facet_grid(~Position)
CrossTable(df$how.many.years,df$what.EAP,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(df$how.many.years,df$how.to.access.EAP,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")

