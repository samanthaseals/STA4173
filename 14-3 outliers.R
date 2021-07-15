library(tidyverse)
library(ggpubr)

airport <- c(seq(1,22,1))

revenue <- c(233, 272, 253, 296, 268, 296, 276, 235, 253, 233, 240,  267, 338,
             243, 252, 269, 242, 233, 234, 450, 340, 200)

distance <- c(233, 209, 206, 232, 125, 245, 213, 134, 140, 165, 234, 205, 214,
              183, 230, 238, 144, 220, 170, 170, 290, 340)

population <- c(56, 74, 67, 78, 73, 54, 100, 98, 95, 81, 52, 96, 96, 73,
                55, 91, 64, 60, 60, 240, 70, 75)

data <- tibble(airport, revenue, distance, population)

# scatter plots
ggplot(data, aes(x = distance, y = revenue)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(x = "Distance", y = "Revenue") + 
  theme(text = element_text(size=20))

#ggsave("/Users/sseals/Desktop/L99fig3.png")

ggplot(data, aes(x = population, y = revenue)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(x = "Population", y = "Revenue") + 
  theme(text = element_text(size=20))

#ggsave("/Users/sseals/Desktop/L99fig4.png")

# model of interest
m1 <- lm(revenue ~ distance + population, data = data)
summary(m1)

# check for outliers
data$outlier <- abs(rstandard(m1))>2.5

data2 <- filter(data, outlier == FALSE)

# model of interest, without outlier
m2 <- lm(revenue ~ distance + population, data = data2)
summary(m2)
