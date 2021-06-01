library(tidyverse)

x <- c(100, 102, 103, 101, 105, 100, 99, 105)
y <- c(257, 264, 274, 266, 277, 263, 258, 275)

data <- as_tibble(x, y)

p <- ggplot(data = data, aes(x = x, y = y)) +
  geom_point(size=5) +
  ylab("Distance (yards)") +
  xlab("Club-Head Speed (mph)") +
  theme_minimal() +
  theme(text = element_text(size=20))
        
p
        
#ggsave("/Users/sseals/Desktop/L61fig1.png")
        