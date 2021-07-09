library(tidyverse)

#### EXAMPLE 1 ####

# create data
g1 <- c(17.6, 20.2, 19.5, 11.3, 13.0, 16.3, 15.3, 16.2, 12.2, 14.8, 21.3, 22.1, 16.9, 17.6, 18.4)
g2 <- c(17.3, 19.1, 18.4, 11.5, 12.7, 15.8, 14.9, 15.3, 12.0, 14.2, 21.0, 21.0, 16.1, 16.7, 17.5)

garage <- tibble(g1, g2)

# the following section is to help with hand calculations
###########################################
garage$d <- garage$g1 - garage$g2
mean(garage$d)
sd(garage$d)
###########################################

# hypothesis test
t.test(garage$g1, garage$g2, paired = TRUE, alternative = "greater")
# first argument is the variable for group 1
# second argument is the variable for group 2
# paired = TRUE tells it to do the paired t-test (otherwise, it will do the independent t-test for two groups)
# alternative = "greater" specifies a right tail test
# warning: this produces a one-sided CI (we want a two-sided CI)

# two-sided CI
t.test(garage$g1, garage$g2, paired = TRUE, alternative = "two")
# just calling t.test() again but specify alternative = "two" for a two-sided test
# this will produce the two-sided CI
# note that we are only interested in the CI here as the hypothesis test 
# results come from the previous t.test() call

#### EXAMPLE 2 ####

# create data
s1 <- c(0.177, 0.210, 0.186, 0.189, 0.198, 0.194, 0.160, 0.163, 0.166, 0.152, 0.190, 0.172)
s2 <- c(0.179, 0.202, 0.208, 0.184, 0.215, 0.193, 0.194, 0.160, 0.209, 0.164, 0.210, 0.197)

student <- tibble(s1, s2)

# the following section is to help with hand calculations
###########################################
student$d <- student$s1 - student$s2
mean(student$d)
sd(student$d)
###########################################

# hypothesis test and two-side CI
t <- t.test(student$s1, student$s2, paired = TRUE, alternative = "two")
# note that we can use this for both the hypothesis test and CI
# because we were interested in a two-tailed test
