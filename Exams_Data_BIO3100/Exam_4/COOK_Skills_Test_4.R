#I. 	
#Your first task, though, is to create separate histograms of the DNA concentrations for Katy and Ben. 
#Make sure to add nice labels to these (x-axis and main title).
library(tidyverse)
DNA <- read.delim("./DNA_Conc_by_Extraction_Date.csv", sep=",", stringsAsFactors = FALSE)
summary(DNA)

jpeg("COOK_HistKaty.jpeg", width = 4, height = 4, units = 'in', res = 300)
hist(DNA$DNA_Concentration_Katy, main = "Katy's DNA concentrations", xlab ="Concentration", ylab = "Frequency")
dev.off()

jpeg("COOK_HistBen.jpeg", width = 4, height = 4, units = 'in', res = 300)
hist(DNA$DNA_Concentration_Ben, main = "Ben's DNA concentrations", xlab ="Concentration", ylab = "Frequency")
dev.off()


#II. 	
#Your second task is to look at DNA concentrations from the different extraction years. 
#I'd like you to re-create these exactly, including the labels.
#This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of vectors that are given to it.
class(DNA$Year_Collected)
class(DNA$DNA_Concentration_Katy)
DNA$Year_Collected <- as.factor(DNA$Year_Collected)

plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Katy,main = "Katy's Extractions", xlab ="YEAR", ylab = "DNA Concentration")

plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Ben, main = "Ben's Extractions", xlab ="YEAR", ylab = "DNA Concentration")

#III.
#save those two images in YOUR Exam_1 directory. Name them similarly to how I named mine, but with your LASTNAME

jpeg("COOK_Plot1.jpeg", width = 4, height = 4, units = 'in', res = 300)
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Katy,main = "Katy's Extractions", xlab ="YEAR", ylab = "DNA Concentration")
dev.off()

jpeg("COOK_Plot2.jpeg", width = 4, height = 4, units = 'in', res = 300)
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Ben, main = "Ben's Extractions", xlab ="YEAR", ylab = "DNA Concentration")
dev.off()


#IV.
#Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... with a plot or with summary stats or both.
#It looks like Ben had consistently higher DNA yields than Katy did...but surely it wasn't uniformly better, right? With some samples, he only had a marginal improvement over Katy.
#With other samples, he had a relatively massive improvement over her.
#Your task here is to write some code that tells us: in which extraction YEAR, was Ben's performance the lowest RELATIVE TO Katy's performance?

plot(DNA$DNA_Concentration_Katy,DNA$DNA_Concentration_Ben)
bensummary <- summary(DNA$DNA_Concentration_Ben)
katysummary <- summary(DNA$DNA_Concentration_Katy)
cbind(bensummary,katysummary)

#compare year by year
ben_vs_katy <- (DNA$DNA_Concentration_Ben - DNA$DNA_Concentration_Katy)
min(ben_vs_katy) #lowest relative difference btwn ben and katy 
bens_worst_relative <- which(ben_vs_katy == min(ben_vs_katy))
DNA[bens_worst_relative, "Year_Collected"] #the year is 2000



#  V.
#Do another subset of the data for me. Subset the data frame so it's just the "Downstairs" lab.
#Now, make a scatterplot of the downstairs lab data such that "Date_Collected" is on the x-axis and "DNA_Concentration_Ben" is on the y-axis. 
#Save this scatterplot as "Ben_DNA_over_time.jpg" in your Exam_1 directory. See the file "Downstairs.jpg" for an example of how yours should look.
#If it looks different, you might need to do some class conversions so the plot() function treats things correctly. HintHintHint: POSIXct
down <- DNA %>% filter(Lab == "Downstairs")
down$Date_Collected <- as.POSIXct(down$Date_Collected)
jpeg("Ben_DNA_over_time.jpeg", width = 4, height = 4, units = 'in', res = 300)
plot(down$Date_Collected, down$DNA_Concentration_Ben, main= "Ben DNA Over Time ", xlab= "Date_Collected",
     ylab = "DNA_Concentration_Ben")
dev.off()


#VI.
#Make a new data frame (just using Ben's values) that has one column containing the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  
#You will need to find a way to take the average of Ben's DNA values in each separate year. 

newdat <- DNA %>% group_by(Year_Collected) %>% summarise(ave=mean(DNA_Concentration_Ben))

newdat %>% filter(ave == max(newdat$ave)) #year collected 2007. average 1.46

write.csv(newdat, "./Ben_Average_Conc.csv",row.names = FALSE)





