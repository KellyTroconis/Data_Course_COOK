#load packeges 

library(carData)
carData::
?MplsStops

library(tidyverse)
library(skimr)
library(plotly)


df <- MplsStops
df2 <- MplsDemo

#quik look at data
skim(df)
skim(df2)

#viacle searshs base in rates 

ggplot(df, aes(x=race)) +
  geom_histogram(stat ="count")
  
ggplot(df, aes(x=lat, y=long, color=race)) +
  geom_point(alpha =.1) #alpha is the transparency of the dots

names(df)
names(df2)

#need to combine demographic info with stop info 
df3 <- full_join(df, df2, by="neighborhood")


p <- ggplot(df3, aes(x=lat, y=long, color=black, size=collegeGrad)) +
  geom_point(alpha =.5)

ggplotly(p) #interactive plot 

#geaom density and hex 
ggplot(df3, aes(x=lat, y=long, fill= race)) +
  geom_hex(alpha=.5)

ggplot(df3, aes(x=lat, y=long, color= race)) + #? doesnt work 
  geom_density_2d(linemitre = 20)


ggplot(df2, aes(x=white, y=collegeGrad)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df2, aes(x=foreignBorn, y=collegeGrad)) + 
  geom_point() +
  geom_smooth(method = "lm") #correlation 

options(scipen = 999) #change the scientific notation to actual numbers 
ggplot(df2, aes(x=foreignBorn, y=collegeGrad)) + 
  geom_smooth(method = "lm", color= "pink") +
  geom_point(aes(size=hhIncome), color= "purple", alpha=.5) +
  labs(size="Household Income", title = "College and Race", x= "%Foreign-Born", y= "%College Graduates") +
  theme_bw() #ggplot2 themer packge 


df4 <- carData::Friendly
?Friendly
ggplot(df4, aes(x=condition, y=correct, fill=condition)) +
  geom_violin() + geom_jitter(height = 0) +
  geom_boxplot(alpha=.25) + theme_bw()

df5 <- carData::Chile
?Chile
skim(df5)

ggplot(df5, aes(x=sex, y=income)) +
  geom_violin()

#filter just the points we want to see 
df5 %>% filter(vote %in% c("N","Y")) %>% 
  ggplot(aes(x=statusquo, y=age, color=vote)) +
  geom_point(alpha=.5) + 
  theme_minimal() +
  facet_wrap(~region) + #multiple plots at once 
  

ggplot(df5, aes(x=age, y=statusquo)) +
  geom_smooth(method = "lm") + geom_point()

pal <- c("#3a4494","#a62330")
ggplot(df5, aes(x=statusquo, fill= sex)) +
  geom_density(alpha=.5) + facet_wrap(~region) + labs(x="Statutus Quo Score", y="Density", fill= "Sex") + theme_minimal() + 
  scale_fill_manual(name = "sex", labels = c("Female", "Male"), values = pal2 ) #changes in the leyend  

library(RColorBrewer)
RColorBrewer::brewer.pal(name = "Spectral", n=5)
pal2 <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4", "#2B83BA")


ggplot(df5, aes(x=income, fill= sex)) +
  geom_density(alpha=.5) + facet_wrap(~region)

ggplot(df5, aes(x=income, y=statusquo)) +
  geom_smooth(method = "lm") + geom_point()











