library(tidyverse)

df <- read.delim("./Data/ITS_mapping.csv")

summary(df)
glimpse(df)


table(df$Ecosystem)


png(filename = "./silly_boxplot.png")
plot(x=df$Ecosystem,y=df$Lat)
dev.off()