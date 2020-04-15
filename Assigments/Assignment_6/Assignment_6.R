library(tidyverse)
library(skimr)
library(plotly)
library(RColorBrewer)
library(colorspace)
library(colorblindr)
library(patchwork)

devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")
library(colorspace)
library(colorblindr)
library(ggimage)

data("mtcars")
str(mtcars)
?mtcars
library(knitr)
kable(c("Column     Name      Description",
        "[, 1]	      mpg     	 Miles/(US) gallon",
        "[, 2]	      cyl     	 Number of cylinders",
        "[, 3]	      disp     	 Displacement (cu.in.)",
        "[, 4]	      hp     	 Gross horsepower",
        "[, 5]	      drat     	 Rear axle ratio",
        "[, 6]	      wt     	 Weight (1000 lbs)",
        "[, 7]	      qsec     	 1/4 mile time",
        "[, 8]	      vs     	 V/S",
        "[, 9]	      am     	 Transmission (0 = automatic, 1 = manual)",
        "[,10]	      gear     	 Number of forward gears",
        "[,11]	      carb     	 Number of carburetors"))

# Your task is to Write an R script that:

#1.  loads the mtcars data set
data("mtcars")
str(mtcars)

#2.  subsets the mtcars dataframe to include only **automatic transmissions**
df <- subset(mtcars,am == 0)
getwd()
#3.  saves this new subset as a new file called "automatic_mtcars.csv" in your Assignment_5 directory
?write.csv
write.csv(df,file = "./Assignment_6/automatic_mtcars.csv")

#4.  plots the effect of horsepower on miles-per-gallon (update plot to have meaningful labels and title)
pal = c("#c4a113","#c1593c","#643d91","#820616","#477887","#688e52",
        "#12aa91","#705f36","#8997b2","#753c2b","#3c3e44","#b3bf2d",
        "#82b2a4","#894e7d","#a17fc1","#262a8e","#abb5b5","#000000")
p1 <- ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point() + geom_smooth(method = "lm", color="Green") +
  theme_minimal() + 
  labs(x="Gross_Horsepower", y= "Miles/(US)_Gallon", title = "Effect of Horsepower on Miles-per-gallon")

p1 + scale_color_manual(values=pal) #? why is this not working? 

#5.  saves this plot as a png image called "mpg_vs_hp_auto.png" in your Assignment_5 directory
p1
ggsave(filename = "./mpg_vs_hp_auto.png", plot = p1, device="png")

#6.  plots the effect of weight on miles-per-gallon (with improved labels, again)
p2 <- ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() + geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(x="Weight_(1000 lbs)", y= "Miles/(US)_Gallon", title = "Effect of Weight_(1000 lbs) on Miles-per-gallon") 

#7.  saves this second plot as a **tiff** image called "mpg_vs_wt_auto.tiff" in your Assignment_5 directory
p2
ggsave(filename = "./mpg_vs_wt_auto.tiff", plot = p2, device = "tiff")

#8.  subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
df1 <- subset(mtcars,disp <= 200)

#9.  saves that new subset as a csv file called mtcars_max200_displ.csv
write.csv(df,file = "./Assignment_6/mtcars_max200_displ.csv")

#10. includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
max(mtcars$hp)
max(df$hp)
max(df1$hp)

#11. prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt

maximums <- c(max(mtcars$hp),
              max(df$hp),
              max(df1$hp))

names(maximums) <- c("original", "automatic", "max200")

write.table(maximums, file = "./Assignment_6/hp_maximums.txt")
