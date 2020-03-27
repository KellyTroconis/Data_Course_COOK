library(readxl)
library(tidyverse)

df <- read.csv("./police.csv")

names(df)


dfy <- subset(df, select = c(3:25))
dfb <- subset(df, select = c(3:4, 26:31)) 
dfr <- subset(df, select = c(3:4, 32:44))
dfo <- subset(df, select = c(3:4, 45:54))
dfp <- subset(df, select = c(3:4, 55))
