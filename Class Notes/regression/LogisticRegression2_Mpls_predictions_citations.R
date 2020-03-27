library(carData)
library(modelr)
library(tidyverse)

##load data
data("MplsDemo")
data("MplsStops")

##clean up for modeling

MplsStops$vehicleSearch#is a factor it does not works for logistic regressions
MplsStops$vehicleSearch <- as.character(MplsStops$vehicleSearch)
MplsStops$vehicleSearch[MplsStops$vehicleSearch == "NO"] <- FALSE
MplsStops$vehicleSearch[MplsStops$vehicleSearch == "YES"] <- TRUE
table(MplsStops$vehicleSearch)

MplsStops$citationIssued <- as.character(MplsStops$citationIssued)
MplsStops$citationIssued[MplsStops$citationIssued == "NO"] <- FALSE
MplsStops$citationIssued[MplsStops$citationIssued == "YES"] <- TRUE
MplsStops$citationIssued[is.na(MplsStops$citationIssued)] <- FALSE
table(MplsStops$citationIssued)

names(MplsStops)
#remove rows with missing gender info
MplsStops$gender <- as.character(MplsStops$gender)
MplsStops <- MplsStops[which(MplsStops$gender != "Unknown"),]

#join both of our data frames to have more predictors 
dat <- full_join(MplsStops,MplsDemo,by= "neighborhood")
#now that is joined it needs more cleaning 
#from character to logical
dat$vehicleSearch <- as.logical(dat$vehicleSearch)
dat$citationIssued <- as.logical(dat$citationIssued)

##modeling
names(dat)
mod1 <- glm(data=dat, formula= vehicleSearch ~ race + gender + hhIncome, family= "binomial")

mod2 <- glm(data=dat, formula= citationIssued ~ race + gender + hhIncome, family= "binomial")

mod3 <- glm(data=dat, formula= vehicleSearch ~ race + gender + hhIncome + black, family= "binomial")

#adding predictions
dat2 <- add_predictions(dat,mod1, type="response")
dat3 <- add_predictions(dat,mod2, type="response")
dat4 <- add_predictions(dat,mod3, type="response")

ggplot(dat2, aes(x=hhIncome, y=pred, color=race)) + geom_smooth(se=FALSE) +
  theme_minimal() + labs(y="Predicted probability of viacle search") +
  facet_wrap(~gender, drop = TRUE) + theme(axis.text.x = element_text(angle = 60))

summary(mod1)#coeffitients: since race was cathegorical all the races are being compare to "black"
#it shows if it is signifincanly different from it. 
#native american shows to not be signifficantly different 
#gender is being compare. Males are compare to females and it shows to be very significant 

ggplot(dat3, aes(x=hhIncome, y=pred, color=race)) + geom_smooth(se=FALSE) +
  theme_minimal() + labs(y="Predicted probability of citation issued") +
  facet_wrap(~gender) + theme(axis.text.x = element_text(angle = 60))
summary(mod2)#in this case latino is not significantly different from black 
#house hold income increase the probability of citation issuance 

ggplot(dat4, aes(x=black, y=pred, color=race)) + geom_smooth(se=FALSE) +
  theme_minimal() + labs(y="Predicted probability of citation issued",
                         x="Proportion of black residents in stop neighborhood") +
  facet_wrap(~gender) + theme(axis.text.x = element_text(angle = 60))
summary(mod3)#native american not significant
#probability of citation issued increases as the proportion of black neighborhood increases regardless of race

























 