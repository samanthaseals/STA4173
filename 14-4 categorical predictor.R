library(tidyverse)
library(palmerpenguins)
library(fastDummies)

# pull in penguin dataset
data <- penguins

# create dummy variables for species
# species_Adelie, species_Chinstrap, and species_Gentoo
data <- dummy_cols(data, select_columns = c("species"))

# model
m1 <- lm(bill_length_mm ~ flipper_length_mm + species_Adelie + species_Chinstrap, data = data)
summary(m1)