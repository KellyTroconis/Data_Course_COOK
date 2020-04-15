library(tidyverse)

df <- read.delim("./../../Data_BIO3100/ITS_mapping.csv")

summary(df)
summary(df$Lat)
summary(df$Lat)[c(3,5)]
glimpse(df)


table(df$Ecosystem)
plot(x=df$Ecosystem,y=df$Lat, col=df$Ecosys_Type)

png(filename = "./silly_boxplot.png")
plot(x=df$Ecosystem,y=df$Lat)
dev.off()
