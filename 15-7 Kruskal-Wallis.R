library(tidyverse)
library(pgirmess)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

HDL <- c()

age <- c(rep("20 to 29", 12), 
         rep("40 to 49", 12),
         rep("60 to 69", 12))

data <- tibble(age, HDL)

m1 <- aov(HDL ~ age, data = data)
almost_sas(m1)

kruskal.test(HDL ~ age, data = data)

kruskalmc(HDL ~ age, data = data)
