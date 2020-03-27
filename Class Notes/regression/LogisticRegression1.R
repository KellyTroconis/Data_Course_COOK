#Logistic regresssion

library(tidyverse)
library(modelr)
library(GGally)

dat <- read.csv("./../Assigments/Data/GradSchool_Admissions.csv", stringsAsFactors = FALSE)
#in this data what we are looking for admition (our dependeble variable)
#its shows admition with 1 and 0, = True or False
#which makes modeling different from linear regression

ggpairs(dat)
#this shows that gre and gpa has a correlation

names(dat) 

dat$admit <- as.logical(dat$admit)
dat$rank <- factor(dat$rank)
#this allows for a cleaner data set 

#mod1 <- glm(formula = admit ~ gre + gpa + rank, data = dat)
#summary(mod1)... this way it does not make sence because we dont have a linear regration. 
#We are trying to predict if this is true or false. 
# for this to work we need to add one more thing... Family

mod1 <- glm(formula = admit ~ gre + gpa + rank, data = dat, family = "binomial")
summary(mod1)

dat2 <- add_predictions(dat, mod1, type = "response")
#type = "response" this means we want our predictions to be in the same scale 
#as our response variable in this case is provability

ggplot(dat2, aes(x=gpa, y=pred, color=rank)) +
  geom_point()

ggplot(dat2, aes(x=gre, y=pred, color=rank)) +
  geom_point()

ggplot(dat2, aes(x=gpa, y=pred, color=rank)) +
  geom_smooth(method = "lm")

#things to remember: logistic regression TRUE FALSE variables.
#Use: gml... family = "binomial".... type = "response"


ggplot(dat, aes(x=gpa, y=admit)) + geom_point()
ggplot(dat, aes(x=gre, y=admit)) + geom_point()
#this is reality, the previous predics reality 

#in this part we are making hypotherical students in a new data frame 
names(dat)#remember to keep the names the same as previous 
newdata <- data.frame(gre=c(400,500,600,700), #this is looking at 4 hypotherical people
                      gpa=c(3.5,2.5,4,3.75),
                      rank=c("4","4","4","4"))

add_predictions(newdata, mod1, type = "response")


