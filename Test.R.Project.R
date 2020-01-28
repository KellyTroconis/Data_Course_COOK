# my new file
library(tidyverse)

yedata("Loblolly")
Loblolly
?Loblolly

class(Loblolly$Seed)
class(Loblolly$height)
num1 <- Loblolly$Seed[1]
num1+1


str(Loblolly)
summary(Loblolly)
table(Loblolly$Seed)
levels(Loblolly$Seed)
as.numeric(Loblolly$Seed)
as.character(Loblolly$Seed)




hist(Loblolly$height,breaks = 84)
plot(x=Loblolly$height,y=Loblolly$age,col=Loblolly$Seed, pch=19, main = "trees", xlab ="tree age",ylab = "tree H")

hist(Loblolly$age)
