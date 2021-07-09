library(tidyverse)

age <- c(25, 25, 28, 32, 32, 32, 38, 42, 48, 51, 51, 58, 62, 65)
chol <- c(180, 195, 186, 180, 210, 197, 239, 183, 204, 221, 243, 208, 228, 269)

data <- tibble(age, chol)

summary(lm(chol ~ age, data = data))

confint(lm(chol ~ age, data = data))
