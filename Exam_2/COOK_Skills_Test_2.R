library(tidyverse)
library(skimr)
library(plotly)
library(scales)

#I.Load the landdata-states.csv file into R
#Re-create the graph shown in "fig1.png"
#Export it to your Exam_2 folder as LASTNAME_Fig_1.jpg (note, that's a jpg, not a png)
#To change the y-axis values to plain numeric, add options(scipen = 999) to your script

#II.What is "NA Region???" Write some code to show which state(s) are found in the "NA" region   


df1 <- read.csv("./landdata-states.csv")
names(df1)
options(scipen = 999)
p1 <- ggplot(df1, aes(x=Date, y=Land.Value, color=region)) + 
  geom_smooth() +
  labs(x="Year", y= "Land Value (USD)", color="Region") +
  theme_minimal()
p1
ggsave(filename = "./COOK_Fig_1.jpg", plot = p1, device = "jpg")

#II.What is "NA Region???" Write some code to show which state(s) are found in the "NA" region
df1[is.na(df1$region),] %>% subset(select = State) %>% summary()
q2 <- df1[is.na(df1$region),] %>% subset(select = State) %>% summary()

#subset(q2, select = State) %>% summary()
#df1 %>% is.na(df1$region) %>%
#?subset
#which(q2$State)
#df1[is.na(df1$region),] %in% summary[(df1$State)] 
#summary[(df1$State)] <-  df1[is.na(df1$region),]
#sum(is.na(df1$region))
#which(is.na(df1$Region) == df1$State)
#df1$State[(is.na(df1$region))]

#III.The rest of the test uses another data set. The unicef-u5mr.csv data. Get it loaded and take a look.
#It's not exactly tidy. You had better tidy it!
df2 <- read.csv("./unicef-u5mr.csv")
names(df2) #needs to be converted to long format 
long <- gather(df2,key=Year , value=U5MR,2:67) #rank=new name salary= variables 
new_year_name <- str_remove(long$Year, c("U5MR."))
long$Year <-  new_year_name
long$Year <- as.numeric(long$Year)

#IV. Re-create the graph shown in fig2.png
#Export it to your Exam_2 folder as LASTNAME_Fig_2.jpg (note, that's a jpg, not a png)

p2 <- ggplot(long, aes(x=Year, y=U5MR, color=Continent)) +
  geom_point(size= 2) +
  labs(y="MortalityRate") +
  theme_minimal()
p2
ggsave(filename = "./COOK_Fig_2.jpg", plot = p2, device = "jpg")

#IV.Re-create the graph shown in fig3.png
#Note: This is a line graph of average mortality rate over time for each continent 
#(i.e., all countries in each continent, yearly average), this is NOT a geom_smooth() 
#Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png)

p3 <- long %>% group_by(Continent, Year) %>%
  summarise(Mean = mean(U5MR, na.rm = TRUE)) %>%
  ggplot(aes(x=Year, y=Mean, color=Continent)) +
  geom_line(size= 2) +
  labs(y="Mean Mortality Rate (deaths per 1000 live births)") +
  theme_minimal()
p3
ggsave(filename = "./COOK_Fig_3.jpg", plot = p3, device = "jpg")

#V.Re-create the graph shown in fig4.png
#Note: The y-axis shows proportions, not raw numbers
#This is a scatterplot, faceted by region
#Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png)

skim(long)
glimpse(long)
options(scipen = 999)
p4 <- ggplot(long, aes(x=Year, y=U5MR)) +
  geom_point(color="Blue", alpha= .5) + 
  scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-3, accuracy = 0.1)) + 
  labs(y="Mortality Rate") +
  theme_minimal() + 
  facet_wrap(~Region) + 
  theme(strip.background = element_rect(size = 1))
p4
ggsave(filename = "./COOK_Fig_4.jpg", plot = p4, device = "jpg")











