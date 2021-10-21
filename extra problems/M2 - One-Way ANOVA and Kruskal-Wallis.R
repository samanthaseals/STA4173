library(tidyverse)
library(multcomp)
library(broom)
library(car)
library(pgirmess)

##### PROBLEM 1 #####

acidity <- c(5.41, 5.39, 4.90, 5.14, 4.80, 5.24,
             4.87, 5.18, 4.52, 5.12, 4.89, 5.06,
             5.46, 5.89, 5.57, 5.15, 5.45, 5.30)

state <- c(rep("Alaska", 6), rep("Florida", 6), rep("Texas", 6))

data <- tibble(state, acidity)

# group means and variances
data %>%
  group_by(state) %>%
  summarise(mean = mean(acidity), 
            var = var(acidity)) 

# overall mean
mean(acidity)

# make "state" a factor variable
data$state <- as_factor(data$state)

# model to plug in to other functions
m1 <- lm(acidity ~ state, data=data)

# ANOVA
anova(m1)

# Tukey
m2 <- aov(acidity ~ state, data=data)
TukeyHSD(m2)$state

# create the graph
tky <- as.data.frame(TukeyHSD(m2)$state)
tky$pair <- rownames(tky)

ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.05, 1), 
                           label=c("p < 0.05", "p ≥ 0.05")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
  geom_point(aes(pair, diff)) +
  xlab("Parwise Comparison") +
  ylab("Difference") +
  labs(colour="") +
  theme_minimal() +
  theme(text = element_text(size=20))

##### PROBLEM 2 #####

births <- c(10456, 10023, 10691, 10283, 10265, 11189, 11198, 11465,
            11621, 11944, 11045, 12927, 12577, 11753, 12509, 13521,
            11084, 11570, 11346, 11875, 12193, 11593, 11216, 11818,
            11171, 11745, 12023, 12433, 12132, 11903, 11233, 12543,
            11545, 12321, 11749, 12192, 12422, 11627, 11624, 12543)

day <- c(rep("M", 8), rep("T", 8), rep("W", 8), rep("R", 8), rep("F", 8))

data <- tibble(day, births)

# group means and variances
data %>%
  group_by(day) %>%
  summarise(mean = mean(births), 
            var = var(births)) 

# overall mean
mean(births)

# make "state" a factor variable
data$day <- as_factor(data$day)

# model to plug in to other functions
m1 <- lm(births ~ day, data=data)

# ANOVA
anova(m1)

# Tukey
m2 <- aov(births ~ day, data=data)
TukeyHSD(m2)$day

# create the graph
tky <- as.data.frame(TukeyHSD(m2)$day)
tky$pair <- rownames(tky)

ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.05, 1), 
                           label=c("p < 0.05", "p ≥ 0.05")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
  geom_point(aes(pair, diff)) +
  xlab("Parwise Comparison") +
  ylab("Difference") +
  labs(colour="") +
  theme_minimal() +
  theme(text = element_text(size=20))

##### PROBLEM 3 #####

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

##### PROBLEM 4 #####

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
