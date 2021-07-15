library(tidyverse)

# create data
strength <- c(15.4, 12.9, 17.2, 16.6, 19.3,
              17.2, 14.3, 17.6, 21.6, 17.5,
               5.5,  7.7, 12.2, 11.4, 16.4,
              11.0, 12.4, 13.5,  8.9,  8.1)

system <- c(rep("Cojet",5), rep("Silistor",5), rep("Cimara",5), rep("Ceramic",5))

data <- tibble(system, strength)

# the following section is to help with hand calculations
###########################################
# group means and variances
data %>%
  group_by(system) %>%
    summarise(mean = mean(strength), 
              var = var(strength)) 

# overall mean
mean(strength)
###########################################

# ANOVA
# we are actually running this through a linear model function first,
# but as we will learn in the future, ANOVA = linear model/regression
m1 <- lm(strength ~ system, data=data)
# on the left of the ~ is the outcome of interest, 
# on the right of the ~ is the grouping variable (or predictor)

# run the model results through the anova() function to 
# get the ANOVA table
anova(m1)

# note that it does not provide SST in the event that you need it 
# just add all of the SS to get SST
# (same thing for df)
