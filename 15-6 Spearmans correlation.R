library(tidyverse)
library(ggpubr)

# create data
x <- c(100, 102, 103, 101, 105, 100, 99, 105)
y <- c(257, 264, 274, 266, 277, 263, 258, 275)

data <- as_tibble(x, y)

# find Spearman's correlation
cor(x, y, method = c("spearman"))

# scatterplot of data
ggplot(data, aes(x = x, y = y)) + 
  geom_point(size=5) +
  ylab("Distance (yards)") +
  xlab("Club-Head Speed (mph)") +
  theme_minimal() +
  theme(text = element_text(size=20))

# check normality assumption on Pearson's
clubhead <- ggplot(data, aes(sample = x)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Club Head Speed (mph)") +
  theme(text = element_text(size=20))

distance <- ggplot(data, aes(sample = y)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Distance (yards)") +
  theme(text = element_text(size=20))

ggarrange(clubhead, distance, ncol=2, nrow=1)
