library(tidyverse)
library(multcomp)
library(broom)

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
strength <- c(15.4, 12.9, 17.2, 16.6, 19.3,
              17.2, 14.3, 17.6, 21.6, 17.5,
              5.5,  7.7, 12.2, 11.4, 16.4,
              11.0, 12.4, 13.5,  8.9,  8.1)

system <- c(rep("Cojet",5), rep("Silistor",5), rep("Cimara",5), rep("Ceramic",5))

data <- tibble(system, strength)
data <- data %>% mutate (system = factor(system))

# create model
m1 <- lm(strength ~ system, data=data)
# on the left of the ~ is the outcome of interest, 
# on the right of the ~ is the grouping variable (or predictor)

# run model results through the function we defined to create graph
almost_sas(m1)

# top left is the scatterplot of residuals (variance)
# top right is the histogram of the residuals (normality)
# bottom left is the QQ plot (normality)
# bottom right is a smoothed function in the shape of the histogram 
# (use it to help interpret the histogram)
