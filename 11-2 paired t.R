library(tidyverse)

#### EXAMPLE 1 ####
g1 <- c(17.6, 20.2, 19.5, 11.3, 13.0, 16.3, 15.3, 16.2, 12.2, 14.8, 21.3, 22.1, 16.9, 17.6, 18.4)
g2 <- c(17.3, 19.1, 18.4, 11.5, 12.7, 15.8, 14.9, 15.3, 12.0, 14.2, 21.0, 21.0, 16.1, 16.7, 17.5)

garage <- tibble(g1, g2)

# garage$d <- garage$g1 - garage$g2
# mean(garage$d)
# sqrt(var(garage$d))

# hypothesis test
t.test(garage$g1, garage$g2, paired = TRUE, alternative = "greater")

# two-sided CI
t.test(garage$g1, garage$g2, paired = TRUE, alternative = "two")


#### EXAMPLE 2 ####

s1 <- c(0.177, 0.210, 0.186, 0.189, 0.198, 0.194, 0.160, 0.163, 0.166, 0.152, 0.190, 0.172)
s2 <- c(0.179, 0.202, 0.208, 0.184, 0.215, 0.193, 0.194, 0.160, 0.209, 0.164, 0.210, 0.197)

student <- tibble(s1, s2)

# student$d <- student$s1 - student$s2
# mean(student$d)
# sqrt(var(student$d))

# hypothesis test and two-side CI
t <- t.test(student$s1, student$s2, paired = TRUE, alternative = "two")
