library(tidyverse)
library(ggpubr)

ill <- c(640, 160, 1280, 320, 80, 640, 640, 160, 1280, 640, 160)
healthy <- c(10, 320, 160, 160, 320, 320, 10, 320, 320, 80, 640)

i <- tibble(ill)
h <- tibble(healthy)

p1 <- ggplot(data=i, aes(sample=ill)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Ill")+
  theme_minimal()

p2 <- ggplot(data=h, aes(sample=healthy)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Healthy")+
  theme_minimal()

both <- ggarrange(p1, p2, ncol=2, nrow=1)
both

#ggsave("/Users/sseals/Desktop/L71fig1.png")

outcome <- c(640, 160, 1280, 320, 80, 640, 640, 160, 1280, 640, 160,
             10, 320, 160, 160, 320, 320, 10, 320, 320, 80, 640)
healthy <- c(rep(0,11), rep(1,11))

data <- tibble(outcome, healthy)

wilcox.test(data$outcome~data$healthy, exact = FALSE, alternative="greater")
# assign the alternative "greater" because we want to know if the 
# ill group (healthy=0) is greater than the healthy group (healthy=1)
# R will do the comparison in the order that it sees the grouping
# and it sees the 0 group first
