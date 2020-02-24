library(tidyverse)

data("diamonds")
glimpse(diamonds)
?diamonds


ggplot(diamonds, aes(y=price, x=carat)) + geom_hex(alpha=.25) +
  geom_smooth(method = "lm", aes(color=clarity)) +
  coord_cartesian(ylim = c(0,20000))

#price is the dependent variable... price as a fuction of ....
mod1 <- lm(data = diamonds, price ~ carat)
mod2 <- lm(data = diamonds, price ~ carat + clarity)

summary(mod1)
summary(mod2)

newdata <- data.frame(carat = c(2,2.1,2.3))
predict(mod1, newdata = newdata)


newdata <- data.frame(carat = c(2,2.1,2.3),
                      clarity = 'I1')
predict(mod2, newdata = newdata)


ggplot(diamonds, aes(y=price, x=carat)) + geom_hex(alpha=.25) +
  geom_smooth(method = "lm", aes(color=clarity)) +
  coord_cartesian(ylim = c(0,20000)) +
  facet_wrap(~cut)


library(beepr)
beep(sound = "mario")







