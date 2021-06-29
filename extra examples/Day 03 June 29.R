library(tidyverse)

##### PROBLEM 1 #####

rating <- c("E", "G", "F", "F", "P", "E", "G", "G", "F", "F", "F", "G", "E", "P", "F",
            "F", "F", "G", "G", "F", "F", "P", "F", "G", "G", "E", "F", "G", "P", "P")

# create a numeric version
rating2 <- if_else(rating == "E", 1, 
           if_else(rating == "G", 2,
           if_else(rating == "F", 3, 4)))

critic <- c(rep("A", 15), rep("B", 15))

data <- tibble(critic, rating, rating2)

data$rank <- rank(data$rating2, ties.method = "average")

sums <- data %>% 
  group_by(critic) %>%
  summarize_at(vars(rank),
               list(name = sum))

wilcox.test(data$rating2~data$critic, exact = FALSE, alternative="two.sided")

##### PROBLEM 2 #####

avis <- c(67.54, 97.16, 103.43, 38.79, 87.90, 109.86, 109.35, 82.86, 112.65, 116.46, 105.49)
hertz <- c(59.14, 97.15, 103.14, 36.63, 87.80, 110.09, 109.35, 83.03, 113.32, 114.53, 107.04)

data <- tibble(avis, hertz)

data$diff <- data$avis - data$hertz
data$absdiff <- abs(data$diff)

# remove diff=0
data <- data %>% filter(diff != 0)

data$rank <- rank(data$absdiff, ties.method = "average")
data$rank <- if_else(data$diff<0, data$rank*(-1), data$rank)

data$sign <- if_else(data$diff<0, "-", "+")

data %>% 
  group_by(sign) %>%
  summarize_at(vars(rank),
               list(name = sum))

wilcox.test(data$avis, data$hertz, paired = TRUE, alternative = "less", exact = FALSE)