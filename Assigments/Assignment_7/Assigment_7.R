library(tidyverse)

##########################
#        Part 1          #
##########################

# load data (wide format)
utah = read.csv("./../../Data_BIO3100/Utah_Religions_by_County.csv")

# subset to only counties with buddhists observed
#buddhist = utah[utah$Buddhism.Mahayana > 0,]
buddhist <- filter(utah, Buddhism.Mahayana > 0)

# order rows by population (descending)
#buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]
buddhist <- buddhist %>% arrange(desc(Pop_2010))

#I wasn't sure if you wanted step by step but i guess i could write just one line of code
buddhist <- utah %>% filter(Buddhism.Mahayana > 0) %>% arrange(desc(Pop_2010))

# write this new dataframe to a file
write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)

## get group summaries of religiousity based on population ##

# divide each county into one of six groups based on populations
# note: keep these two lines the same in your updated code!
groups = kmeans(utah$Pop_2010,6) # clusters data into 6 groups based on proximity to mean of potential groups
utah$Pop.Group = groups$cluster # assigns a new variable to utah giving group for each county

# subset to each group and find summary stats on Religiosity for each
#group1 = mean(utah[utah$Pop.Group == 1,]$Religious)
#group2 = mean(utah[utah$Pop.Group == 2,]$Religious)
#group3 = mean(utah[utah$Pop.Group == 3,]$Religious)
#group4 = mean(utah[utah$Pop.Group == 4,]$Religious)
#group5 = mean(utah[utah$Pop.Group == 5,]$Religious)
#group6 = mean(utah[utah$Pop.Group == 6,]$Religious)

sb1 <- utah %>% group_by(Pop.Group) %>% summarise(Mean.Religiosity= mean(Religious))

# same, but mean population
#group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
#group2.pop = mean(utah[utah$Pop.Group == 2,]$Pop_2010)
#group3.pop = mean(utah[utah$Pop.Group == 3,]$Pop_2010)
#group4.pop = mean(utah[utah$Pop.Group == 4,]$Pop_2010)
#group5.pop = mean(utah[utah$Pop.Group == 5,]$Pop_2010)
#group6.pop = mean(utah[utah$Pop.Group == 6,]$Pop_2010)

sb2 <- utah %>% group_by(Pop.Group) %>% summarise(Mean.Pop= mean(Pop_2010))

# make data frame of each group and mean religiosity
#religiosity = data.frame(Pop.Group = c("group1","group2","group3","group4","group5","group6"),
                         #Mean.Religiosity = c(group1,group2,group3,group4,group5,group6),
                         #Mean.Pop = c(group1.pop,group2.pop,group3.pop,group4.pop,group5.pop,group6.pop))
religiosity <- full_join(sb1, sb2)
religiosity # take quick look at resulting table

# order by decreasing population
#religiosity = religiosity[order(religiosity$Mean.Pop, decreasing = TRUE),]
religiosity <- arrange(religiosity, desc(Mean.Pop))
religiosity # take quick look at resulting table

#last two steps can be done toguether as: 
#religiosity <- full_join(sb1, sb2) %>% arrange(desc(Mean.Pop))

#Part2 on canvas 
