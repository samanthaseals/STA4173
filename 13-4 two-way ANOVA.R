library(tidyverse)

weight <- c(157.78, 136.79, 138.84,
            139.72, 125.47, 117.14,
            129.35, 110.73, 118.38,
            137.07, 146.28, 130.27,
            117.46, 128.54,  99.16,
             97.43, 125.26, 115.42)

# create data
age <- c(rep("Aged 20 - 29", 9), rep("Aged 30 - 39", 9))

diet <- c(rep(c(rep("Diet 1", 3), rep("Diet 2", 3), rep("Diet 3", 3)),2))

data <- tibble(age, diet, weight)

# the following section is to help with hand calculations
###########################################
data %>% group_by(diet) %>% summarize(sum = sum(weight))
data %>% group_by(age) %>% summarize(sum = sum(weight))
data %>% group_by(age, diet) %>% summarize(sum = sum(weight))
sum(weight)
###########################################

# ANOVA with interaction

# define model
m1 <- lm(weight ~ age + diet + age:diet, data = data)
# on the left of the ~ is the outcome of interest, 
# on the right of the ~ is the grouping variables
# we add factors to our model by using +
# we specify interaction terms with : between the two variables 

# run model results through the anova() function
anova(m1)

# ANOVA without interaction

# define model
m2 <- lm(weight ~ age + diet, data = data)
# on the left of the ~ is the outcome of interest, 
# on the right of the ~ are the grouping variables
# note that to remove the interaction we just remove that term from the model

# run model results through the anova() function
anova(m2)
