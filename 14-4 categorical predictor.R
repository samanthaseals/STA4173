library(tidyverse)
library(palmerpenguins)
library(fastDummies)

# pull in penguin dataset
data <- penguins

# create dummy variables for species
data <- dummy_cols(data, select_columns = c("species"))
# you can include multiple variables in the select_columns option
# this created species_Adelie, species_Chinstrap, and species_Gentoo

# model of interest
# remember that we must exclude one of the categories as a reference group
# here, we are excluding Gentoos, so we will be comparing 
# Adelies to Gentoos and, separately, Chinstraps to Gentoos
m1 <- lm(bill_length_mm ~ flipper_length_mm + species_Adelie + species_Chinstrap, data = data)

# see results through summary() function
summary(m1)