library(tidyverse)

weight <- c(157.78, 136.79, 138.84,
            139.72, 125.47, 117.14,
            129.35, 110.73, 118.38,
            137.07, 146.28, 130.27,
            117.46, 128.54,  99.16,
            97.43, 125.26, 115.42)

age <- c(rep("Aged 20 - 29", 9), rep("Aged 30 - 39", 9))

diet <- c(rep(c(rep("Diet 1", 3), rep("Diet 2", 3), rep("Diet 3", 3)),2))

data <- tibble(age, diet, weight)

means <- data %>% group_by(diet, age) %>% summarize(mean = mean(weight))

diet1 <- c(144.47, 137.87)
diet2 <- c(127.44, 115.05)
diet3 <- c(119.49, 112.70)

age_g <- c("20-29", "30-39")

graph <- tibble(age_g, diet1, diet2, diet3)

ggplot(data=graph, aes(x=age_g)) +
  geom_line(aes(y=diet1), color="black", group=1) +
  geom_line(aes(y=diet2), color="black", group=1) +
  geom_line(aes(y=diet3), color="black", group=1) +
  theme_minimal() +
  labs(x = "Age Group", y = "Average Birth Weight") +
  geom_text(aes(x = "30-39" , y = 137, label = "Diet 1")) +
  geom_text(aes(x = "30-39" , y = 114, label = "Diet 2")) + 
  geom_text(aes(x = "30-39" , y = 112, label = "Diet 3")) +
  theme(text = element_text(size=20))

#ggsave("/Users/sseals/Desktop/L88fig1.png")