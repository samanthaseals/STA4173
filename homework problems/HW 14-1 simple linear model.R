library(tidyverse)

x <- c(3, 4, 5, 7, 8)
y <- c(5, 6, 8, 11, 14)

data <- tibble(x, y)

# model of interest
m1 <- lm(y ~ x, data = data)
summary(m1)
# summary will give us b0, b1, s_b1, p-value for t-test

# s_e
data$resid <- residuals(m1)
sqrt(sum(data$resid^2/3))

