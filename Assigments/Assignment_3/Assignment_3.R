# Assignment Week 3

library(tidyverse)


# topics:   type conversions, factors, plot(), making a data frame from "scratch",
#           reordering, 


# vector operations!

vector1 = c(1,2,3,4,5,6,7,8,9,10)
vector2 = c(5,6,7,8,4,3,2,1,3,10)

vector1*vector2

list.files()

dat = read.csv("./../../Data_BIO3100/thatch_ant.csv")
names(dat)

#why are these plots different???

#one is numeric and the other is with factors 
plot(x=dat$Headwidth..mm., y=dat$Mass)
plot(x=dat$Size.class, y=dat$Mass)


#check the classes of these vectors
class(dat$Headwidth..mm.)
class(dat$Size.class)

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat)
str(dat)

# Two of them are "Factor" ....why is the column "Headwidth" a factor? It looks numeric!

# we can try to coerce one format into another with a family of functions
# as.factor, as.matrix, as.data.frame, as.numeric, as.character, as.POSIXct, etc....

#make a numeric vector to play with:
nums = c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric

# convert to a factor
as.factor(nums) # show in console
nums_factor = as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it

#check it out
plot(nums) 
plot(nums_factor)
# take note of how numeric vectors and factors behave differently in plot()

# Let's modify and save these plots. Why not!?
?plot()
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label")


?
  jpeg("practice1.jp")
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label")
dev.off()



# back to our ant data...
dat$Headwidth
levels(dat$Headwidth) # levels gives all the "options" of a factor you feed it

# I notice a couple weird ones in there: "" and "41mm"
# The "" means a missing value, basically. The "41mm" sure looks like a data entry error.
# It should probably be "41.000"

# FIND WHICH ONES HAVE "41mm"
bad41 <- which(grepl("41mm",dat$Headwidth))

# CONVERT THOSE TO "41.000"
dat$Headwidth[bad41] <- "41.000"

# DO THE SAME FOR "", BUT CONVERT THOSE TO "NA"
levels(dat$Headwidth)
dat$Headwidth[dat$Headwidth==""] <- NA

# NOW, REMOVE ALL THE ROWS OF "dat" THAT HAVE AN "NA" VALUE
dat_no_na <- na.omit(dat)


# NOW, CONVERT THAT PESKY "Headwidth" COLUMN INTO A NUMERIC VECTOR WITHIN "dat"
levels(dat_no_na$Headwidth)
dat_no_na$Headwidth <- factor(dat_no_na$Headwidth, levels = unique(dat_no_na$Headwidth))

as.numeric(as.character(dat_no_na$Headwidth)) #samething
dat_no_na$Headwidth <- dat_no_na$Headwidth %>% as.character() %>% as.numeric() #samething in tidyverswe

class(dat_no_na$Headwidth)

#taking a look at the new data
plot(dat_no_na$Headwidth,dat_no_na$Mass)

# LET'S LEARN HOW TO MAKE A DATA FRAME FROM SCRATCH... WE JUST FEED IT VECTORS WITH NAMES!

# make some vectors *of equal length* (or you can pull these from existing vectors)
col1 = c("hat", "tie", "shoes", "bandana")
col2 = c(1,2,3,4)
col3 = factor(c(1,2,3,4)) # see how we can designate something as a factor             

# here's the data frame command:
data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # colname = vector, colname = vector....
df1 = data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # assign to df1
df1 # look at it...note column names are what we gave it.



# Make a data frame from the first 20 rows of the ant data that only has "Colony" and "Mass"
# save it into an object called "dat3"
cols = c("Colony", "Mass")

dat3 <- dat[1:20,cols]



###### WRITING OUT FILES FROM R #######
?write.csv()
getwd()

# Write your new object "dat3" to a file named "LASTNAME_first_file.csv" in your PERSONAL git repository
write.csv(dat3, file = "COOK_first_file.cvs")


### for loops in R ###

#simplest example:
for(i in 1:10){
  print(i) # i could say i+10 and it would add to the value of i
}

#another easy one
for(i in levels(dat_no_na$Size.class)){
  print(i)
}

# can calculate something for each value of i ...can use to subset to groups of interest
for(i in levels(dat_no_na$Size.class)){
  print(mean(dat_no_na[dat_no_na$Size.class == i,"Mass"]))
}

# more complex:
# define a new vector or data frame outside the for loop first
new_vector = c() # it's empty
# also define a counter
x = 1

for(i in levels(dat_no_na$Size.class)){
  new_vector[x] = mean(dat_no_na[dat_no_na$Size.class == i,"Mass"])
  x = x+1 # add 1 to the counter (this will change the element of new_vector we access each loop)
}

#check it
new_vector



# PUT THIS TOGETHER WITH THE LEVELS OF OUR FACTOR SO WE HAVE A NEW DATA FRAME:
# FIRST COLUMN WILL BE THE FACTOR LEVELS....
# SECOND COLUMN WILL BE NAMED "MEAN" AND WILL BE VALUES FROM  new_vector

#fill it in
size_class_mean_mass = data.frame(Size_Class = levels(dat_no_na$Size.class), MEAN = new_vector)

dat_no_na %>% group_by(Size.class) %>% summarize(mean_mass = mean(Mass), mean_headwith = mean(Headwidth), med_headwith = median(Headwidth))

dat_no_nasummary <- dat_no_na %>% group_by(Size.class) %>% summarize(mean_mass = mean(Mass), mean_headwith = mean(Headwidth), med_headwith = median(Headwidth)) 

############ YOUR HOMEWORK ASSIGNMENT ##############

# 1.  Make a scatterplot of headwidth vs mass. See if you can get the points to be colored by "Colony"
plot(dat_no_na$Headwidth,dat_no_na$Mass, col=dat_no_na$Colony, pch=19, main = "Colonies", xlab = "Headwidth", ylab = "Mass")


# 2.  Write the code to save it (with meaningful labels) as a jpeg file
jpeg("Ant_Colonies_HeadwidthvsMass.jpeg")
plot(dat_no_na$Headwidth,dat_no_na$Mass, col=dat_no_na$Colony, pch=19, main = "Colonies", xlab = "Headwidth", ylab = "Mass")

dev.off()

# 3.  Subset the thatch ant data set to only include ants from colony 1 and colony 2
?subset
col1_2 <- subset(dat_no_na, Colony==1 | Colony==2)
c

# 4.  Write code to save this new subset as a .csv file
write.csv(col1_2, file = "colonies_1_and_2.cvs")

# 5.  Upload this R script (with all answers filled in and tasks completed) to canvas
# I should be able to run your R script and get all the plots created and saved, etc.