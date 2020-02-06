#I. 	
#Once you get the csv file loaded into an R object as a data frame, feel free to do some exploratory visualizations or summaries to get a feel for the data if you like.
#Your first task, though, is to create separate histograms of the DNA concentrations for Katy and Ben. Make sure to add nice labels to these (x-axis and main title).
library(tidyverse)
DNA <- read.delim("./DNA_Conc_by_Extraction_Date.csv", sep=",", stringsAsFactors = FALSE)
summary(DNA)
hist(DNA$DNA_Concentration_Katy, main = "Katy's DNA concentrations", xlab ="Concentration", ylab = "Frequency")
hist(DNA$DNA_Concentration_Ben, main = "Ben's DNA concentrations", xlab ="Concentration", ylab = "Frequency")



#II. 	
#Your second task is to look at DNA concentrations from the different extraction years. 
#One way to do this is a separate figure for each student is demonstrated in those two files:	ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg 
#Open those files in some image viewing program and take a look. I'd like you to re-create these exactly, including the labels.
#This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of vectors that are given to it.
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Katy,main = "Katy's Extractions", xlab ="YEAR", ylab = "DNA Concentration")
class(DNA$Year_Collected)
class(DNA$DNA_Concentration_Katy)
DNA$Year_Collected <- as.factor(DNA$Year_Collected)
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Ben, main = "Ben's Extractions", xlab ="YEAR", ylab = "DNA Concentration")

#III.
#Once you have your code for creating the figures correctly, you need to save those two images in YOUR Exam_1 directory. Name them similarly to how I named mine, but with your LASTNAME
#Make sure your code is saving the files. Don't do it manually with the mouse!
jpeg("COOK_Plot1.jpeg")
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Katy,main = "Katy's Extractions", xlab ="YEAR", ylab = "DNA Concentration")
dev.off()

jpeg("COOK_Plot2.jpeg")
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Ben, main = "Ben's Extractions", xlab ="YEAR", ylab = "DNA Concentration")
dev.off()


#IV.
#Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... with a plot or with summary stats or both.
#It looks like Ben had consistently higher DNA yields than Katy did...but surely it wasn't uniformly better, right? With some samples, he only had a marginal improvement over Katy.
#With other samples, he had a relatively massive improvement over her.
#Your task here is to write some code that tells us: in which extraction YEAR, was Ben's performance the lowest RELATIVE TO Katy's performance?
DNA %>% summarize(DNA_Concentration_Katy, DNA_Concentration_Ben)
                                                    
plot(DNA$DNA_Concentration_Katy,DNA$DNA_Concentration_Ben, col=DNA$DNA_Concentration_Ben)


  
#  V.
#Do another subset of the data for me. Subset the data frame so it's just the "Downstairs" lab.
#Now, make a scatterplot of the downstairs lab data such that "Date_Collected" is on the x-axis and "DNA_Concentration_Ben" is on the y-axis. Save this scatterplot as "Ben_DNA_over_time.jpg" in your Exam_1 directory. See the file "Downstairs.jpg" for an example of how yours should look. If it looks different, you might need to do some class conversions so the plot() function treats things correctly. HintHintHint: POSIXct
plot(x=DNA$Date_Collected, y=DNA$DNA_Concentration_Ben)
class(DNA$Date_Collected)
?POSIXct
DNA$Date_Collected <- as.POSIXct(DNA$Date_Collected)
jpeg("Ben_DNA_over_time.jpg.jpeg")
plot(x=DNA$Date_Collected, y=DNA$DNA_Concentration_Ben, xlab ="Date_Collected", ylab = "DNA_Concentration_Ben")
dev.off()

#VI.
#For this final (BONUS) problem, let's just look at Ben's DNA concentration values. I think Katy messed up her PCRs, and at any rate, we can't use them for sequencing.
#Besides, our original purpose for this experiment was to see if DNA extractions sitting in a freezer degraded over time.
#To that end, I want you to make a new data frame (just using Ben's values) that has one column containing the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  Just to be clear, this data frame should have only 12 rows (one for each year)! You will need to find a way to take the average of Ben's DNA values in each separate year. A for-loop, or repeated subsetting, or some other way...
          
                                        #Once you have this new data frame of averages by year, write some code that shows which extraction year has the highest average DNA concentration (and what that concentration is) and then save the 12-row dataframe as a new csv file called "Ben_Average_Conc.csv"
year= c("Year_Collected")
averarage= c()
data.frame()                                                  

                                                  #VII.
                                                  #Push the following to your github web page in your new Exam_1 directory:
                                                   # 1. Boxplot of DNA concentration values by year for Katy
                                                  #2. Boxplot of DNA concentration values by year for Ben
                                                  #3. The scatterplot called "Ben_DNA_over_time.jpg"
                                                  #4. Your complete R script file, saved as LASTNAME_Skills_Test_1.R
                                                  #5. Your bonus dataframe, if you did the bonus (Section VI)