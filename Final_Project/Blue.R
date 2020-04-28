library(readxl)
library(tidyverse)
library(modelr)
library(GGally)
library(skimr)
library(corrplot)
library(jtools)


dat <- read.csv("./Programs_Blue.cvs")
dat$how.many.years <- factor(dat$how.many.years)
levels(dat$how.many.years) <- c("1-6", "7-13", "14-20", "20+")
CrossTable(dat$how.many.years,dat$way.spiritual,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$how.many.years,dat$extent.spiritual,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$how.many.years,dat$spritual.help,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$how.many.years,dat$spiritual.tolerate,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$how.many.years,dat$spiritual.support,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$how.many.years,dat$chaplain,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
names(dat)
ggpairs(dat)
dat2 <- subset(dat, select = c(3:8))
dat2c <- cor(dat2)
corrplot.mixed(dat2c)

mod1 <- lm(data=dat, formula = spritual.help ~ spiritual.tolerate)
su1<- summary((mod1))$r.squared
effect_plot(mod1, pred = spiritual.tolerate, plot.points = TRUE, jitter = 0)

p1 <- add_predictions(dat,mod1) %>%
  ggplot(aes(x= spiritual.tolerate, fill = factor(spiritual.tolerate))) +
  geom_bar() +
  guides(fill = FALSE) +
  facet_grid(~how.many.years)

p1

ggplot(mod1p, aes(x = whatE, fill = factor(whatE))) +
  geom_bar() +
  guides(fill = FALSE) +
  facet_grid(~Years)


