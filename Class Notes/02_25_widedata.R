library(tidyverse)

#load "wide" data examples

df1 <- read.csv("./../Data_BIO3100/FacultySalaries_1995.csv")
df2 <- read.csv("./../../wide_income_rent.csv")

#make boxplot (xaxis=professor rank, yaxis= Salary)
#one variable per column 
names(df1) #needs to be converted to long format 
gather(df1,key=Rank , value=Salary,5:7) #rank=new name salary= variables 
long <- gather(df1,key=Rank , value=Salary,5:7)
long$Rank #the names are too long for the variable 
#remove what you dont want from the variable 
str_remove(long$Rank, c("Avg")) %>% str_remove("Salary")
#rename 
newranknames <- str_remove(long$Rank, c("Avg")) %>% str_remove("Salary")
#sent to original data
long$Rank <- newranknames
#plot it now is clean up
p1 <- ggplot(long, aes(x=Rank, y=Salary, fill=Rank)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 3) #scale_fill_manual(values= c())
ggsave(filename = "./p1.png", plot = p1, device="png")
p1
gather(df1,key=Rank , value=Comp,9:11)
long2 <- gather(df1,key=Rank , value=Comp,9:11)
long2$Rank
newnameslong2 <- str_remove(long2$Rank, c("Avg")) %>% str_remove("Comp")
long2$Rank <- newnameslong2
p2 <- ggplot(long2, aes(x=Rank, y=Comp, fill=Rank)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 2)

ggsave(filename = "./p2.png", plot = p2, device="png")
p2
#this is better
names(long)
long1.2 <- gather(long, key=Rank, value= Comp, 6:8)
newnameslong1.2 <- str_remove(long1.2$Rank, c("Avg")) %>% str_remove("Comp")
long1.2$Rank <- newnameslong1.2

names(long1.2)
long1.3 <- gather(long1.2, key= Type, value = Dollars, c(12,14))

names(long1.3)
p3 <- ggplot(long1.3, aes(x=Rank, y=Dollars, fill=Rank)) + geom_boxplot() +
  facet_wrap(~Type)
ggsave(filename = "./p3.png", plot = p3, device="png")


df2 <- read.csv("./../Class Notes/wide_income_rent.csv")
names(df2)
gt1 <- gather(df2,key=State , value=Amount,2:53)

p4 <- ggplot(gt1, aes(x=State, y=Amount)) +
  geom_point() + facet_wrap(~variable)
ggsave(filename = "./p4.png", plot = p4, device="png")


#how to chance name of columns 
names(gt1)
gt1 %>% filter(variable=="income")
names(gt1)[1] <- "poop"

