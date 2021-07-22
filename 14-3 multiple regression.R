library(tidyverse)

# create data
age <- c(25, 25, 28, 32, 32, 32, 38, 42, 48, 51, 51, 58, 62, 65)
chol <- c(180, 195, 186, 180, 210, 197, 239, 183, 204, 221, 243, 208, 228, 269)
fat <- c(19, 28, 19, 16, 24, 20, 31, 20, 26, 24, 32, 21, 21, 30)
data <- tibble(age, chol, fat)

# construct regression line
m1 <- lm(chol ~ age + fat, data = data)

# look at results
summary(m1)

# confidence intervals
confint(m1)


