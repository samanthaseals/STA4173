library(tidyverse)

# create data
age <- c(25, 25, 28, 32, 32, 32, 38, 42, 48, 51, 51, 58, 62, 65)
chol <- c(180, 195, 186, 180, 210, 197, 239, 183, 204, 221, 243, 208, 228, 269)
data <- tibble(age, chol)

# model of interest
m1 <- lm(y ~ x, data = data)
# the outcome is on the left of the ~
# the predictor is on the right of the ~

# run results through the confint() function
# to obtain the CI for beta(s); default alpha is 0.05
confint(m1)

# can change alpha by specifying level
# level = 0.99 gives us a 99% CI
confint(m1, level = 0.99)
