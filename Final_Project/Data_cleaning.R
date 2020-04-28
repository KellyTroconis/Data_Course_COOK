library(readxl)
library(tidyverse)
library(modelr)
library(GGally)
library(skimr)

df <- read.csv("./police.csv")

names(df)
df$how.many.years[df$how.many.years == "FALSE"] <- NA
df$likely.to.access.EAP[df$likely.to.access.EAP == "FALSE"] <- NA
df$who.is.peer.support[df$who.is.peer.support == "FALSE"] <- NA
df$likely.to.access[df$likely.to.access == "FALSE"] <- NA
df$how.to.access.peer.sup[df$how.to.access.peer.sup == "FALSE"] <- NA
df$comfort.with.peer[df$comfort.with.peer == "FALSE"] <- NA
df$attending.other.skills.trainings[df$attending.other.skills.trainings == "FALSE"] <- NA
df$resources.at.shift.briefs[df$resources.at.shift.briefs == "FALSE"] <- NA
df$diet.and.nutrition[df$diet.and.nutrition == "FALSE"] <- NA
df$exercise[df$exercise == "FALSE"] <- NA
df$family.skills[df$family.skills == "FALSE"] <- NA
df$support.group[df$support.group == "FALSE"] <- NA
df$marriage.skills[df$marriage.skills == "FALSE"] <- NA
df$personal.finance[df$personal.finance == "FALSE"] <- NA
df$resilencey[df$resilencey == "FALSE"] <- NA
df$reimbursment[df$reimbursment == "FALSE" ] <- NA
df$how.many.years[df$how.many.years == "FALSE"] <- NA
df$way.spiritual[df$way.spiritual == "FALSE"] <- NA
df$extent.spiritual[df$extent.spiritual == "FALSE"] <- NA
df$spritual.help[df$spritual.help == "FALSE"] <- NA
df$spiritual.tolerate[df$spiritual.tolerate == "FALSE"] <- NA
df$spiritual.support[df$spiritual.support == "FALSE"] <- NA
df$how.many.years[df$how.many.years == "FALSE"] <- NA
df$separate.work[df$separate.work == "FALSE"] <- NA
df$falling.asleep[df$falling.asleep == "FALSE"] <- NA
df$sleep.issues.before[df$sleep.issues.before == "FALSE"] <- NA
df$feeling.depressed[df$feeling.depressed == "FALSE"] <- NA
df$frequency.thoughts[df$frequency.thoughts == "FALSE"] <- NA
df$how.many.years[df$how.many.years == "FALSE"] <- NA
df$don.t.know.where[df$don.t.know.where == "FALSE"] <- NA
df$time.off.issues[df$time.off.issues == "FALSE"] <- NA
df$embarressing[df$embarressing == "FALSE"] <- NA
df$less.confidence[df$less.confidence == "FALSE"] <- NA
df$others.who.need [df$others.who.need == "FALSE"] <- NA





dfnN <- na.omit(df)
dfnN$how.many.years <-  factor(dfnN$how.many.years, levels = unique(dfnN$how.many.years)) %>% as.character() %>% as.numeric()
dfnN$likely.to.access.EAP <-  factor(dfnN$likely.to.access.EAP, levels = unique(dfnN$likely.to.access.EAP)) %>% as.character() %>% as.numeric()
dfnN$who.is.peer.support <-  factor(dfnN$who.is.peer.support, levels = unique(dfnN$who.is.peer.support)) %>% as.character() %>% as.numeric()
dfnN$likely.to.access <-  factor(dfnN$likely.to.access, levels = unique(dfnN$likely.to.access)) %>% as.character() %>% as.numeric()
dfnN$how.to.access.peer.sup <-  factor(dfnN$how.to.access.peer.sup, levels = unique(dfnN$how.to.access.peer.sup)) %>% as.character() %>% as.numeric()
dfnN$comfort.with.peer <-  factor(dfnN$comfort.with.peer, levels = unique(dfnN$comfort.with.peer)) %>% as.character() %>% as.numeric()
dfnN$attending.other.skills.trainings <-  factor(dfnN$attending.other.skills.trainings, levels = unique(dfnN$attending.other.skills.trainings)) %>% as.character() %>% as.numeric()
dfnN$resources.at.shift.briefs <-  factor(dfnN$resources.at.shift.briefs, levels = unique(dfnN$resources.at.shift.briefs)) %>% as.character() %>% as.numeric()
dfnN$diet.and.nutrition <-  factor(dfnN$diet.and.nutrition, levels = unique(dfnN$diet.and.nutrition)) %>% as.character() %>% as.numeric()
dfnN$exercise <-  factor(dfnN$exercise, levels = unique(dfnN$exercise)) %>% as.character() %>% as.numeric()
dfnN$family.skills <-  factor(dfnN$family.skills, levels = unique(dfnN$family.skills)) %>% as.character() %>% as.numeric()
dfnN$support.group <-  factor(dfnN$support.group, levels = unique(dfnN$support.group)) %>% as.character() %>% as.numeric()
dfnN$marriage.skills <-  factor(dfnN$marriage.skills, levels = unique(dfnN$marriage.skills)) %>% as.character() %>% as.numeric()
dfnN$personal.finance <-  factor(dfnN$personal.finance, levels = unique(dfnN$personal.finance)) %>% as.character() %>% as.numeric()
dfnN$resilencey <-  factor(dfnN$resilencey, levels = unique(dfnN$resilencey)) %>% as.character() %>% as.numeric()
dfnN$reimbursment <-  factor(dfnN$reimbursment, levels = unique(dfnN$reimbursment)) %>% as.character() %>% as.numeric()
dfnN$way.spiritual <-  factor(dfnN$way.spiritual, levels = unique(dfnN$way.spiritual)) %>% as.character() %>% as.numeric()
dfnN$extent.spiritual <-  factor(dfnN$extent.spiritual, levels = unique(dfnN$extent.spiritual)) %>% as.character() %>% as.numeric()
dfnN$spritual.help <-  factor(dfnN$spritual.help, levels = unique(dfnN$spritual.help)) %>% as.character() %>% as.numeric()
dfnN$spiritual.tolerate <-  factor(dfnN$spiritual.tolerate, levels = unique(dfnN$spiritual.tolerate)) %>% as.character() %>% as.numeric()
dfnN$spiritual.support <-  factor(dfnN$spiritual.support, levels = unique(dfnN$spiritual.support)) %>% as.character() %>% as.numeric()
dfnN$separate.work <-  factor(dfnN$separate.work, levels = unique(dfnN$separate.work)) %>% as.character() %>% as.numeric()
dfnN$falling.asleep <-  factor(dfnN$falling.asleep, levels = unique(dfnN$falling.asleep)) %>% as.character() %>% as.numeric()
dfnN$sleep.issues.before <-  factor(dfnN$sleep.issues.before, levels = unique(dfnN$sleep.issues.before)) %>% as.character() %>% as.numeric()
dfnN$feeling.depressed <-  factor(dfnN$feeling.depressed, levels = unique(dfnN$feeling.depressed)) %>% as.character() %>% as.numeric()
dfnN$frequency.thoughts <-  factor(dfnN$frequency.thoughts, levels = unique(dfnN$frequency.thoughts)) %>% as.character() %>% as.numeric()
dfnN$don.t.know.where <-  factor(dfnN$don.t.know.where, levels = unique(dfnN$don.t.know.where)) %>% as.character() %>% as.numeric()
dfnN$time.off.issues <-  factor(dfnN$time.off.issues, levels = unique(dfnN$time.off.issues)) %>% as.character() %>% as.numeric()
dfnN$embarressing <-  factor(dfnN$embarressing, levels = unique(dfnN$embarressing)) %>% as.character() %>% as.numeric()
dfnN$less.confidence <-  factor(dfnN$less.confidence, levels = unique(dfnN$less.confidence)) %>% as.character() %>% as.numeric()
dfnN$others.who.need <-  factor(dfnN$others.who.need, levels = unique(dfnN$others.who.need)) %>% as.character() %>% as.numeric()
skim(dfnN)
glimpse(dfnN)
names(dfnN)
dat1 <- subset(df, select = c(3:55))
glimpse(dat1)
write_csv(dat1, "./Police_clean.cvs")

#colunm 1 & 2 are not essential for analysis 
#2 is necesasy for the leyend of culunm 3

#Programs Training or service already in place or potentially being considered
dfy <- subset(df, select = c(3:25))
names(dfy)
dfy$how.many.years[dfy$how.many.years == "FALSE"] <- NA
skim(dfy)
levels(dfy$likely.to.access.EAP)
dfy$likely.to.access.EAP[dfy$likely.to.access.EAP == "FALSE"] <- NA
levels(dfy$who.is.peer.support)
dfy$who.is.peer.support[dfy$who.is.peer.support == "FALSE"] <- NA
levels(dfy$likely.to.access.EAP)
dfy$likely.to.access[dfy$likely.to.access == "FALSE"] <- NA
levels(dfy$how.to.access.peer.sup)
dfy$how.to.access.peer.sup[dfy$how.to.access.peer.sup == "FALSE"] <- NA
levels(dfy$comfort.with.peer)
dfy$comfort.with.peer[dfy$comfort.with.peer == "FALSE"] <- NA
levels(dfy$attending.other.skills.trainings)
dfy$attending.other.skills.trainings[dfy$attending.other.skills.trainings == "FALSE"] <- NA
levels(dfy$resources.at.shift.briefs)
dfy$resources.at.shift.briefs[dfy$resources.at.shift.briefs == "FALSE"] <- NA
levels(dfy$diet.and.nutrition)
dfy$diet.and.nutrition[dfy$diet.and.nutrition == "FALSE"] <- NA
levels(dfy$exercise)
dfy$exercise[dfy$exercise == "FALSE"] <- NA
levels(dfy$family.skills)
dfy$family.skills[dfy$family.skills == "FALSE"] <- NA
levels(dfy$support.group)
dfy$support.group[dfy$support.group == "FALSE"] <- NA
levels(dfy$marriage.skills)
dfy$marriage.skills[dfy$marriage.skills == "FALSE"] <- NA
levels(dfy$personal.finance)
dfy$personal.finance[dfy$personal.finance == "FALSE"] <- NA
levels(dfy$resilencey)
dfy$resilencey[dfy$resilencey == "FALSE"] <- NA
levels(dfy$reimbursment)
dfy$reimbursment[dfy$reimbursment == "FALSE" ] <- NA

dfy_noNA <- dfy %>% ! na_if(., "FALSE")
dfy_noNA <- na.omit(dfy)
as.numeric(dfy_noNA$reimbursment)
skim(dfy_noNA)
dfy_noNA$who.is.peer.support <- factor(dfy_noNA$who.is.peer.support, levels = unique(dfy_noNA$who.is.peer.support))
dfy_noNA$who.is.peer.support <- dfy_noNA$who.is.peer.support %>% as.character() %>% as.numeric() 
dfy_noNA$how.many.years <- factor(dfy_noNA$how.many.years, levels = unique(dfy_noNA$how.many.years)) %>% as.character() %>% as.numeric() 
dfy_noNA$likely.to.access.EAP <- factor(dfy_noNA$likely.to.access.EAP, levels = unique(dfy_noNA$likely.to.access.EAP)) %>% as.character() %>% as.numeric()
dfy_noNA$who.is.peer.support <- factor(dfy_noNA$who.is.peer.support, levels = unique(dfy_noNA$what.peer.support)) %>% as.character() %>% as.numeric()
dfy_noNA$how.to.access.peer.sup <- factor(dfy_noNA$how.to.access.peer.sup, levels = unique(dfy_noNA$how.to.access.peer.sup)) %>% as.character() %>% as.numeric()
dfy_noNA$likely.to.access <- factor(dfy_noNA$likely.to.access, levels = unique(dfy_noNA$likely.to.access)) %>% as.character() %>% as.numeric()
dfy_noNA$comfort.with.peer <- factor(dfy_noNA$comfort.with.peer, levels = unique(dfy_noNA$comfort.with.peer)) %>% as.character() %>% as.numeric()
dfy_noNA$attending.other.skills.trainings <- factor(dfy_noNA$attending.other.skills.trainings, levels = unique(dfy_noNA$attending.other.skills.trainings)) %>% as.character() %>% as.numeric()
dfy_noNA$resources.at.shift.briefs <- factor(dfy_noNA$resources.at.shift.briefs, levels = unique(dfy_noNA$resources.at.shift.briefs)) %>% as.character() %>% as.numeric()
dfy_noNA$diet.and.nutrition <- factor(dfy_noNA$diet.and.nutrition, levels = unique(dfy_noNA$diet.and.nutrition)) %>% as.character() %>% as.numeric()
dfy_noNA$exercise <- factor(dfy_noNA$exercise, levels = unique(dfy_noNA$exercise)) %>% as.character() %>% as.numeric()
dfy_noNA$family.skills <- factor(dfy_noNA$family.skills, levels = unique(dfy_noNA$family.skills)) %>% as.character() %>% as.numeric()
dfy_noNA$support.group <- factor(dfy_noNA$support.group, levels = unique(dfy_noNA$support.group)) %>% as.character() %>% as.numeric()
dfy_noNA$marriage.skills <- factor(dfy_noNA$marriage.skills, levels = unique(dfy_noNA$marriage.skills)) %>% as.character() %>% as.numeric()
dfy_noNA$mindfulness <- factor(dfy_noNA$mindfulness, levels = unique(dfy_noNA$mindfulness)) %>% as.character() %>% as.numeric()
dfy_noNA$personal.finance <- factor(dfy_noNA$personal.finance, levels = unique(dfy_noNA$personal.finance)) %>% as.character() %>% as.numeric()
dfy_noNA$resilencey <- factor(dfy_noNA$resilencey, levels = unique(dfy_noNA$resilencey)) %>% as.character() %>% as.numeric()
dfy_noNA$reimbursment <- factor(dfy_noNA$reimbursment, levels = unique(dfy_noNA$reimbursment)) %>% as.character() %>% as.numeric()
write_csv(dfy_noNA, "./Programs_Yellow.cvs")

#Spiritual Impact
dfb <- subset(df, select = c(3:4, 26:31)) 
names(dfb)
skim(dfb)
dfb$how.many.years[dfb$how.many.years == "FALSE"] <- NA
dfb$way.spiritual[dfb$way.spiritual == "FALSE"] <- NA
dfb$extent.spiritual[dfb$extent.spiritual == "FALSE"] <- NA
dfb$spritual.help[dfb$spritual.help == "FALSE"] <- NA
dfb$spiritual.tolerate[dfb$spiritual.tolerate == "FALSE"] <- NA
dfb$spiritual.support[dfb$spiritual.support == "FALSE"] <- NA
dfb_noNA <- na.omit(dfb)
dfb_noNA$how.many.years <- factor(dfb_noNA$how.many.years , levels = unique(dfb_noNA$how.many.years)) %>% as.character() %>% as.numeric()
dfb_noNA$way.spiritual <- factor(dfb_noNA$way.spiritual , levels = unique(dfb_noNA$way.spiritual)) %>% as.character() %>% as.numeric()
dfb_noNA$extent.spiritual <- factor(dfb_noNA$extent.spiritual , levels = unique(dfb_noNA$extent.spiritual)) %>% as.character() %>% as.numeric()
dfb_noNA$spritual.help <- factor(dfb_noNA$spritual.help , levels = unique(dfb_noNA$spritual.help)) %>% as.character() %>% as.numeric()
dfb_noNA$spiritual.tolerate <- factor(dfb_noNA$spiritual.tolerate , levels = unique(dfb_noNA$spiritual.tolerate)) %>% as.character() %>% as.numeric()
dfb_noNA$spiritual.support <- factor(dfb_noNA$spiritual.support , levels = unique(dfb_noNA$spiritual.support)) %>% as.character() %>% as.numeric()
write_csv(dfb_noNA, "./Programs_Blue.cvs")


#PTSD, Depression, Anxiety and Suicidal Measures
dfr <- subset(df, select = c(3:4, 32:44))
names(dfr)
skim(dfr)
dfr$how.many.years[dfr$how.many.years == "FALSE"] <- NA
dfr$separate.work[dfr$separate.work == "FALSE"] <- NA
dfr$falling.asleep[dfr$falling.asleep == "FALSE"] <- NA
dfr$sleep.issues.before[dfr$sleep.issues.before == "FALSE"] <- NA
dfr$feeling.depressed[dfr$feeling.depressed == "FALSE"] <- NA
dfr$frequency.thoughts[dfr$frequency.thoughts == "FALSE"] <- NA
dfr_noNA <- na.omit(dfr)
dfr_noNA$how.many.years <- factor(dfr_noNA$how.many.year, levels = unique(dfr_noNA$how.many.year)) %>% as.character() %>% as.numeric()
dfr_noNA$separate.work <- factor(dfr_noNA$separate.work , levels = unique(dfr_noNA$separate.work)) %>% as.character() %>% as.numeric()
dfr_noNA$falling.asleep <- factor(dfr_noNA$falling.asleep , levels = unique(dfr_noNA$falling.asleep)) %>% as.character() %>% as.numeric()
dfr_noNA$sleep.issues.before <- factor(dfr_noNA$sleep.issues.before , levels = unique(dfr_noNA$sleep.issues.before)) %>% as.character() %>% as.numeric()
dfr_noNA$feeling.depressed <- factor(dfr_noNA$feeling.depressed , levels = unique(dfr_noNA$feeling.depressed)) %>% as.character() %>% as.numeric()
dfr_noNA$frequency.thoughts <- factor(dfr_noNA$frequency.thoughts , levels = unique(dfr_noNA$frequency.thoughts)) %>% as.character() %>% as.numeric()
skim(dfr_noNA)
write_csv(dfr_noNA, "./Programs_Red.cvs")

#Barriers to getting help
dfo <- subset(df, select = c(3:4, 45:54))
skim(dfo)
dfo$how.many.years[dfo$how.many.years == "FALSE"] <- NA
dfo$don.t.know.where[dfo$don.t.know.where == "FALSE"] <- NA
dfo$time.off.issues[dfo$time.off.issues == "FALSE"] <- NA
dfo$embarressing[dfo$embarressing == "FALSE"] <- NA
dfo$less.confidence[dfo$less.confidence == "FALSE"] <- NA
dfo$others.who.need [dfo$others.who.need == "FALSE"] <- NA
dfo_noNA <- na.omit(dfo)
dfo_noNA$how.many.years <- factor(dfo_noNA$how.many.years, levels = unique(dfo_noNA$how.many.years)) %>% as.character() %>% as.numeric()
dfo_noNA$don.t.know.where <- factor(dfo_noNA$don.t.know.where, levels = unique(dfo_noNA$don.t.know.where)) %>% as.character() %>% as.numeric()
dfo_noNA$time.off.issues <- factor(dfo_noNA$time.off.issues, levels = unique(dfo_noNA$time.off.issues)) %>% as.character() %>% as.numeric()
dfo_noNA$embarressing <- factor(dfo_noNA$embarressing, levels = unique(dfo_noNA$embarressing)) %>% as.character() %>% as.numeric()
dfo_noNA$less.confidence <- factor(dfo_noNA$less.confidence, levels = unique(dfo_noNA$less.confidence)) %>% as.character() %>% as.numeric()
dfo_noNA$others.who.need <- factor(dfo_noNA$others.who.need, levels = unique(dfo_noNA$others.who.need)) %>% as.character() %>% as.numeric()
skim(dfo_noNA)
write_csv(dfo_noNA, "./Programs_Orange.cvs")

#coping
dfp <- subset(df, select = c(3:4, 55))
skim(dfp)
dfp$how.many.years[dfp$how.many.years == "FALSE"] <- NA
dfp_noNA <- na.omit(dfp)
dfp_noNA$how.many.years <- factor(dfp_noNA$how.many.years, levels = unique(dfp_noNA$how.many.years)) %>% as.character() %>% as.numeric()
skim(dfp_noNA)
write_csv(dfp_noNA, "./Programs_Purple.cvs")









