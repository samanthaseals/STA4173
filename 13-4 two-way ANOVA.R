library(tidyverse)

weight <- c(157.78, 136.79, 138.84,
            139.72, 125.47, 117.14,
            129.35, 110.73, 118.38,
            137.07, 146.28, 130.27,
            117.46, 128.54,  99.16,
             97.43, 125.26, 115.42)

age <- c(rep("Aged 20 - 29", 9), rep("Aged 30 - 39", 9))

diet <- c(rep(c(rep("Diet 1", 3), rep("Diet 2", 3), rep("Diet 3", 3)),2))

data <- tibble(age, diet, weight)

# ANOVA with interaction
m1 <- lm(weight ~ age + diet + age:diet, data = data)
anova(m1)

# ANOVA without interaction
m2 <- lm(weight ~ age + diet, data = data)
anova(m2)
