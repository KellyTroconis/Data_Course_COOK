library(tidyverse)
library(readxl)
library(skimr)



dat <- read_xlsx("./../Assigments/Data/wide_data_example.xlsx") #excel spreadsheet 
data("iris")
skim(dat)


#find and destry the "?"
dat$`Treatment 1`[dat$`Treatment 1`=="?"] <- NA
#convert treatment 1 to numeric 
dat$`Treatment 1` <- as.numeric(dat$`Treatment 1`)

#overwrite the names of dat with a new vector.. clean up column names 
names(dat) <- c("SampleID", "Treatment1", "Treatment2")

#tidy dat to "long formant" 
long <- gather(dat, key= "Watering", value = "Height", 2:3)

#get ridof "treatment' tex 
long$Watering <- str_replace(long$Watering, "Treatment", "")
#plot
ggplot(long, aes(x=SampleID, y=Height, color=Watering)) + geom_boxplot()
#save clean data.. we dont want thr row names be changed 
write.csv(long,"./long_and_tidy.csv", row.names = FALSE)




