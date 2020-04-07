library(tidyverse)
library(modelr)
library(GGally)
library(broom)
library(patchwork)
library(jtools) 

df <- read.csv("./../../Assigments/Data/GradSchool_Admissions.csv", stringsAsFactors = FALSE)
write.csv(df,"./GradSchool_Admissions.csv")

ggpairs(df)

#this shows that gre and gpa has a correlation

names(dat) 
df$rank <- factor(df$rank)
df$admit <- as.logical(df$admit)

#this allows for a cleaner data set 

mod1 <- glm(formula = admit ~ gre + gpa + rank, data = df, family = "binomial")
effect_plot(mod1, pred = gre, plot.points = TRUE, jitter = 0)
df2 <- add_predictions(df, mod1, type = "response")
ggplot(df2, aes(x=gpa, y=pred, color=rank)) +
  geom_point() + stat_smooth(method="glm", se=FALSE)

mod2 <- glm(formula = admit ~ gre * gpa * rank, data = df, family = "binomial")


mod3 <- glm(formula = admit ~ gre + gpa * rank, data = df, family = "binomial")

mod4 <- glm(formula = admit ~ gre * gpa + rank, data = df, family = "binomial")

summary(mod1)
summary(mod2)##bets
summary(mod3)
summary(mod4)##2best

# Compare models ####
anova(mod1, mod2) # different?
anova(mod1, mod3)
anova(mod2, mod3)
anova(mod3, mod4)

# which has better fit ?
mod1mse <- mean(residuals(mod1)^2)
mod2mse <- mean(residuals(mod2)^2)###1.12
mod3mse <- mean(residuals(mod3)^2)
mod4mse <- mean(residuals(mod4)^2)

dat2 <- add_predictions(df, mod2, type = "response")
names(dat2)

ggplot(dat2, aes(x=gpa, y=pred, color=rank)) +
  geom_point() 

ggplot(dat2, aes(x=gre, y=pred, color=rank)) +
  geom_point() + stat_smooth(method="glm", se=FALSE) 

ggplot(dat2, aes(x=gpa, y=pred, color=rank)) +
  geom_smooth(method = "lm")



ggplot(df, aes(x=gpa, y=admit)) + geom_point() 
ggplot(df, aes(x=rank, y=admit)) + geom_point() 
ggplot(df, aes(x=gre, y=admit)) + geom_point()
#this is reality, the previous predics reality 

#in this part we are making hypotherical students in a new data frame 
names(df)#remember to keep the names the same as previous 
newdata <- data.frame(gre=c(400,545,650,780), #this is looking at 4 hypotherical people
                      gpa=c(3,3.5,4,3.75),
                      rank=c("4","4","4","4"))

dat3 <- add_predictions(newdata, mod1, type = "response")
ggplot(dat3, aes(x=gpa, y=pred, color=rank)) +
  geom_point() + stat_smooth(method="glm", se=FALSE) 


pal <- c("#00D45B","#FFD603","#172A3A","#E71D36","#0072FF")
palette_plot(pal)
