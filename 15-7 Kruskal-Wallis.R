library(tidyverse)
library(pgirmess) 

# define function to graph ANOVA assumptions
# written by student Reid Ginoza
almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

# create data
HDL <- c(54, 43, 38, 30, 61, 53, 35, 34, 39, 46, 50, 35,
         61, 41, 44, 47, 33, 29, 59, 35, 34, 74, 50, 65,
         44, 65, 62, 53, 51, 49, 49, 42, 35, 44, 37, 38)

age <- c(rep("20 to 29", 12), 
         rep("40 to 49", 12),
         rep("60 to 69", 12))

data <- tibble(age, HDL)

# the following section is to help with hand calculations
###########################################
# find ranks
data$rank <- rank(data$HDL, ties.method = "average")

# sum of ranks
data %>% 
  group_by(age) %>%
  summarize_at(vars(rank),
               list(name = sum))
###########################################

# check ANOVA assumptions
almost_sas(lm(HDL ~ age, data = data))

# Kruskal-Wallis
kruskal.test(HDL ~ age, data = data)
# on the left of the ~ is the outcome of interest, 
# on the right of the ~ is the grouping variable 

# Kruskal-Wallis posthoc
kruskalmc(HDL ~ age, data = data)
# on the left of the ~ is the outcome of interest, 
# on the right of the ~ is the grouping variable (or predictor)
