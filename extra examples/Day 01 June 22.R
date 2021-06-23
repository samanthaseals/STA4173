library(tidyverse)

# create dataset
day <- c(504, 267, 220, 322, 538, 542, 428, 481, 413, 302, 602)
day2 <- day^2
data <- tibble(day, day2)

# sums for sample statistics
sum(day)
sum(day2)

# sample statistics
mean(day)
sd(day)

# test statistic
(mean(day)-480)/(sd(day)/sqrt(11))

# confidence interval
2.228*sd(day)/sqrt(11)
mean(day)-2.228*sd(day)/sqrt(11)
mean(day)+2.228*sd(day)/sqrt(11)

# t test
t.test(day, mu = 480, alternative = "less", conf.level = 0.95)

# confidence interval
t.test(day, mu = 480, alternative = "two.sided", conf.level = 0.95)
