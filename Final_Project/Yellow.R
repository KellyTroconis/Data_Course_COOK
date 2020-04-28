library(readxl)
library(tidyverse)
library(modelr)
library(GGally)
library(skimr)
library(corrplot)
library(jtools)

dat <- read.csv("./Programs_Yellow.cvs")
summary(dat)
glimpse(dat)
#how to chance name of columns 
names(dat)
names(dat)[2] <- "Years"
names(dat)[3] <- "whatE"
names(dat)[4] <- "howE"
names(dat)[5] <- "likeE"
names(dat)[6] <- "whatP"
names(dat)[7] <- "whoP"
names(dat)[8] <- "howP"
names(dat)[9] <- "likeP"
names(dat)[10] <- "comfP"
names(dat)[11] <- "skillsT"
names(dat)[12] <- "partT"
names(dat)[13] <- "shift"
names(dat)[14] <- "wellT"
names(dat)[15] <- "diet"
names(dat)[16] <- "exerc"
names(dat)[17] <- "fam"
names(dat)[18] <- "suppG"
names(dat)[19] <- "marry"
names(dat)[20] <- "finance"
names(dat)[21] <- "mind"
names(dat)[22] <- "resi"
names(dat)[23] <- "reimb"
names(dat)
write_csv(dat, "./Programs_Yellow1.cvs")
dat$Position <- factor(dat$Position)
levels(dat$Position) <- c("SwomO", "Disp")
dat$Years <- factor(dat$Years)
levels(dat$Years) <- c("1-6", "7-13", "14-20", "20+")

#distribution
library(gmodels)
CrossTable(dat$Years,dat$whatE,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$Years,dat$howE,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
CrossTable(dat$Years,dat$likeE,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")

chisq.test(table(dat$Years,dat$whatE))
chisq.test(table(dat$Years,dat$howE))
chisq.test(table(dat$Years,dat$likeE))
chisq.test(table(dat$howE,dat$whatE))
chisq.test(table(dat$whatE,dat$likeE))
ggpairs(dat)


dat1 <- select(dat, whatE, howE, likeE)
ggpairs(dat5)
dat5$howE <- as.factor(dat5$howE)
mod1 <- lm(formula = likeE ~ whatE * howE, data = dat)
summary(mod1)#whatE is a better predictor to likeE
mod1mse <- mean(residuals(mod1)^2)
effect_plot(mod1, pred = whatE, plot.points = TRUE, jitter = 0)
mod1p <- add_predictions(dat, mod1)

ggplot(mod1p, aes(x = whatE, fill = factor(whatE))) +
  geom_bar() +
  guides(fill = FALSE) +
  facet_grid(~Years)

dat2 <- select(dat, whatP, whoP, howP, likeP, comfP)
dat2c <- cor(dat2)
corrplot.mixed(dat2c)
ggpairs(dat2)
mod2 <- lm(data=dat2, formula = whatP ~ whoP)
mod3 <- lm(data=dat2, formula = whatP ~ howP)
mod4 <- lm(data=dat2, formula = whoP ~ howP)
mod5 <- lm(data=dat2, formula = likeP ~ comfP)
summary(mod2)
su2<- summary((mod2))$r.squared
su3<- summary((mod3))$r.squared
su4<- summary((mod4))$r.squared
su5<- summary((mod5))$r.squared
mod2mse <- mean(residuals(mod2)^2)
mod3mse <- mean(residuals(mod3)^2)
mod4mse <- mean(residuals(mod4)^2)
mod5mse <- mean(residuals(mod5)^2)
mod6mse <- mean(residuals(mod6)^2)
mod4p <- add_predictions(dat2, mod4)
effect_plot(mod4, pred = howP, plot.points = TRUE, jitter = 0)
ggplot(mod4p, aes(x=howP,)) +
  geom_point(aes(y=whoP)) +
  geom_point(aes(y=pred, color = factor(pred)))


dat4 <- select(dat, skillsT, partT, shift, wellT)
dat4c <- cor(dat4)
corrplot.mixed(dat4c)

dat4 <- select(dat, diet, exerc, fam, suppG, marry, finance, mind, resi, reimb)
dat4c <- cor(dat4)
corrplot.mixed(dat4c)
ggpairs(dat4)









