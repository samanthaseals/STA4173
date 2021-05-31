library(tidyverse)
library(ggpubr)

#### EXAMPLE 1 -- TWO SAMPLE T TEST ####

space <- c(8.59, 6.87, 7.00, 8.64, 7.89, 8.80, 7.43, 9.79, 9.30, 7.21, 6.85, 8.03, 6.39, 7.54)
s <- as_tibble(space)
earth <- c(8.65, 7.62, 7.33, 6.99, 7.44, 8.58, 8.40, 8.55, 9.88, 9.66, 8.70, 9.94, 7.14, 9.14)
e <- as_tibble(earth)

space_rat <- ggplot(s, aes(sample = value)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Space Rats") +
  theme(text = element_text(size=20))

earth_rat <- ggplot(e, aes(sample = value)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Earth Rats") +
  theme(text = element_text(size=20))

both <- ggarrange(space_rat, earth_rat, ncol=2, nrow=1)
both
#ggsave("/Users/sseals/Desktop/L68fig1.png")


#### EXAMPLE 2 -- PAIRED T TEST ####
g1 <- c(17.6, 20.2, 19.5, 11.3, 13.0, 16.3, 15.3, 16.2, 12.2, 14.8, 21.3, 22.1, 16.9, 17.6, 18.4)
g2 <- c(17.3, 19.1, 18.4, 11.5, 12.7, 15.8, 14.9, 15.3, 12.0, 14.2, 21.0, 21.0, 16.1, 16.7, 17.5)

garage <- tibble(g1, g2)

garage$d <- garage$g1 - garage$g2

p <- ggplot(garage, aes(sample = d)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  theme(text = element_text(size=20))
p

#ggsave("/Users/sseals/Desktop/L68fig2.png")
