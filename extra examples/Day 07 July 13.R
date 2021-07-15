library(tidyverse)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

chol <- c(180, 192, 205, 226, 218, 231,
          175, 193, 213, 222, 203, 185)

age <- c(rep("18-34", 2), rep("35-54", 2), rep("55+", 2), rep("18-34", 2), rep("35-54", 2), rep("55+", 2))

sex <- c(rep("F", 6), rep("M", 6))

data <- tibble(sex, age, chol)

# sums
data %>% group_by(sex) %>% summarize(sum = sum(chol))
data %>% group_by(age) %>% summarize(sum = sum(chol))
data %>% group_by(age, sex) %>% summarize(sum = sum(chol))
sum(chol)

# ANOVA with interaction
m1 <- lm(chol ~ age + sex + age:sex, data = data)
anova(m1)

# ANOVA without interaction
m2 <- lm(chol ~ age + sex, data = data)
anova(m2)

# Tukey's test for significant main effects
m3 <- aov(data=data, chol ~ age + sex)
tky <- as.data.frame(TukeyHSD(m3)$age)
tky$pair <- rownames(tky)

# graph results
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

# ANOVA assumptions
almost_sas(m3)

##### PROBLEM 2 #####

attn <- c(19, 36, 40, 30,  4, 10, 30,  5, 34, 21,
          19, 35, 22, 28,  1, 27, 27, 16,  3, 18,
          37,  6, 28,  4, 32, 16,  8, 41, 29, 18,
          39, 18, 32, 22, 16,  2, 36, 43,  7, 16,
          30, 47,  6, 27, 44, 26, 33, 48, 23, 21,
          51, 52, 43, 48, 39, 33, 56, 43, 40, 51)

age <- c(rep("5-6 years", 10), rep("7-8 years", 10), rep("9-10 years", 10), rep("5-6 years", 10), rep("7-8 years", 10), rep("9-10 years", 10))

product <- c(rep("breakfast cereal", 30), rep("video game", 30))

data <- tibble(product, age, attn)

# sums
data %>% group_by(age) %>% summarize(sum = sum(attn))
data %>% group_by(product) %>% summarize(sum = sum(attn))
data %>% group_by(age, product) %>% summarize(sum = sum(attn))
sum(attn)

# ANOVA with interaction
m1 <- lm(attn ~ age + product + age:product, data = data)
anova(m1)

# profile plot
cereal <- c(229, 196, 219)
game <- c(231, 305, 456)

age_g <- c("5-6 years", "7-8 years", "9-10 years")

graph <- tibble(age_g, cereal, game)

ggplot(data=graph, aes(x=age_g)) +
  geom_line(aes(y=cereal), color="black", group=1) +
  geom_line(aes(y=game), color="black", group=1) +
  theme_minimal() +
  labs(x = "Age Group", y = "Average Attention Span") +
  geom_text(aes(x = "9-10 years" , y = 456, label = "Video Game")) +
  geom_text(aes(x = "9-10 years" , y = 219, label = "Breakfast Cereal")) + 
  theme(text = element_text(size=20))

# ANOVA assumptions
almost_sas(m1)
