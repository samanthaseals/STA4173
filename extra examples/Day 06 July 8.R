library(tidyverse)
library(car)
library(pgirmess)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

##### PROBLEM 1 #####

chest <- c(33, 40, 28, 30, 34, 34, 31,
           29, 28, 27, 30, 26, 33, 36,
           29, 36, 35, 32, 34, 42, 29)

size <- c(rep("Car", 7), rep("Van", 7), rep("SUV", 7))

data <- tibble(size, chest)

# find ranks
data$rank <- rank(data$chest, ties.method = "average")

# sum of ranks
data %>% 
  group_by(size) %>%
  summarize_at(vars(rank),
               list(name = sum))

# perform K-W
kruskal.test(chest ~ size, data = data)

# check assumptions
almost_sas(lm(chest ~ size, data = data))

##### PROBLEM 2 #####

score <- c(578, 548, 548, 530, 521, 502, 555, 492,
           568, 563, 530, 535, 571, 561, 569, 513,
           506, 458, 518, 456, 485, 513, 480, 491)

country <- c(rep("Canada", 8), rep("Denmark", 8), rep("US", 8))

data <- tibble(country, score)

# find ranks
data$rank <- rank(data$score, ties.method = "average")

# sum of ranks
data %>% 
  group_by(country) %>%
  summarize_at(vars(rank),
               list(name = sum))

# perform K-W
kruskal.test(score ~ country, data = data)

# check assumptions
almost_sas(lm(score ~ country, data = data))

# posthoc test
kruskalmc(score ~ country, data = data)

# medians
data %>% 
  group_by(country) %>%
  summarize_at(vars(score),
               list(name = median))
