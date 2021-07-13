library(tidyverse)

outcome <- c(860,	1005,	875,
              853,	990,	940,
              865,	1020,	841,
              905,	1030,	820,
              916,	1020,	875,
              890,	1020,	870,
              854,	1065,	881,
              860,	1005,	835)

milk <- c(rep(c("skim", "mixed", "whole"), 8))

data <- tibble(milk, outcome)

m1 <- lm(outcome ~ milk, data=data)
test <- anova(m1)

means <- data %>%
  group_by(milk) %>%
  summarise(mean = mean(outcome)) 

den <- sqrt((824.8869/2)*(1/8+1/8))

q_mixed_skim = (1019.375 - 875.375)/den
q_mixed_whole = (1019.375 - 867.125)/den
q_skim_whole = (875.375 - 867.125)/den

q_mixed_skim
q_mixed_whole
q_skim_whole
