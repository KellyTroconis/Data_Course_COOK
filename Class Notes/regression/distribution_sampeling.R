?rnorm
?rbinom
?runif

set.seed(123)
a <- rnorm(1000000, mean = 0, sd = 1)
a
b <- rnorm(100000, mean = 5, sd= 1)
b
hist(a)
hist(b)

df <- data.frame(a=a, b=b) %>% gather(key = object, value = value, 1:2)

ggplot(df, aes(x=value, fill= object)) + geom_density(alpha=.5)
pnorm(7, mean = 5, sd= 1, lower.tail = FALSE)#calculates the area under a curve, probability 
ggplot(df, aes(x=value, fill= object)) + geom_histogram(alpha=.5)


set.seed(123)
a <- rnorm(10, mean = 0, sd = 1)
a
b <- rnorm(10, mean = 5, sd= 1)
b
df <- data.frame(a=a, b=b) %>% 
  gather(key = object, value = value, 1:2)
ggplot(df, aes(x=value, fill= object)) + geom_density(alpha=.5)
t.test(a,b)#different btw means... its dependent on sample size 

