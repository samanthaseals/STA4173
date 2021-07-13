library(tidyverse)

# create data
x <- c(100, 102, 103, 101, 105, 100, 99, 105)
y <- c(257, 264, 274, 266, 277, 263, 258, 275)

data <- as_tibble(x, y)

# find Spearman's correlation
cor(x, y, method = c("spearman"))
