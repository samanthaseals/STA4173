library(tidyverse)

# function to construct graph for assumptions
almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

# create data
age <- c(25, 25, 28, 32, 32, 32, 38, 42, 48, 51, 51, 58, 62, 65)
chol <- c(180, 195, 186, 180, 210, 197, 239, 183, 204, 221, 243, 208, 228, 269)

data <- tibble(age, chol)

# model of interest
m1 <- lm(chol ~ age, data = data)

# create plot
almost_sas(m1)
# we plug in the model results that we want to assess assumptions on