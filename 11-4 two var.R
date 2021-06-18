library(tidyverse)

rate <- c(1.93, 31.11, -7.60, 9.72, 11.64, 4.87, 5.18, 8.91, 23.13, -5.56,
          1.15, -1.92, -0.55, 5.29, 4.68, -2.32, -5.88, 9.00, 3.23, -5.19, -3.26, -0.60, 0.98, 4.80)

stock <- c(rep("Cisco", 10), rep("GE", 14))

data <- tibble(stock, rate)

sd <- data %>% group_by(stock) %>% summarize(stdev = sd(rate))

var.test(rate ~ stock, data=data, alternative = "two.sided")