library(tidyverse)
library(ggpubr)

day <- seq(1,14,1)
WBA <- c(8.9, 6.3, 6.2, 7.2, 2.8, 3.3, 23.6, 6.0, 15.6, 5.2, 6.3, 10.1, 4.0, 8.4)
MCD <- c(8.5, 7.6, 8.3, 10.4, 2.5, 2.6, 3.5, 4.7, 9.0, 6.0, 5.6, 5.0, 4.4, 5.6)

data <- tibble(day, WBA, MCD)

data$diff <- data$WBA - data$MCD

p <- ggplot(data=data, aes(sample=diff)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("WBA - MCD")+
  theme_minimal()

p

#ggsave("/Users/sseals/Desktop/L73fig1.png")

data$absdiff <- abs(data$diff)
data$rank <- rank(data$absdiff, ties.method = "average")
data$rank <- if_else(data$diff<0, data$rank*(-1), data$rank)

data$sign <- if_else(data$diff<0, "-", "+")

data %>% 
  group_by(sign) %>%
  summarize_at(vars(rank),
               list(name = sum))
