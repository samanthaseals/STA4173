library(tidyverse)
library(multcomp)
library(broom)

strength <- c(15.4, 12.9, 17.2, 16.6, 19.3,
              17.2, 14.3, 17.6, 21.6, 17.5,
              5.5,  7.7, 12.2, 11.4, 16.4,
              11.0, 12.4, 13.5,  8.9,  8.1)

system <- c(rep("Cojet",5), rep("Silistor",5), rep("Cimara",5), rep("Ceramic",5))

data <- tibble(system, strength)

data <- data %>% mutate (system = factor(system))

m1 <- lm(strength ~ system, data=data)

summary(glht(m1, linfct = mcp(system = "Tukey")))

plot(confint(glht(m1, linfct = mcp(system = "Tukey"))) )
