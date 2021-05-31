libarary(tidyverse)

x <- c(100, 102, 103, 101, 105, 100, 99, 105)
y <- c(257, 264, 274, 266, 277, 263, 258, 275)

data <- as_tibble(x, y)

# mean(x)
# mean(y)
# sqrt(var(x))
# sqrt(var(y))

m1 <- lm(y ~ x, data = data)

data$pred <- predict(m1)

p <- ggplot(data = data, aes(x = x, y = y)) +
  geom_point(size=5) +
  geom_line(aes(y = pred)) +
  ylab("Distance (yards)") +
  xlab("Club-Head Speed (mph)") +
  theme_minimal() +
  theme(text = element_text(size=20))

p

#ggsave("/Users/sseals/Desktop/L63fig1.png")