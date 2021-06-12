library(tidyverse)

scores <- c(82, 77, 90, 71, 62, 68, 74, 84, 94, 88)

mean(scores)

median(scores)

var(scores)

# std. dev.
sqrt(var(scores))

# 5 number summary + mean
summary(scores)

IQR(scores)
