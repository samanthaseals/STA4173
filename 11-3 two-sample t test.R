library(tidyverse)

# create data
rbc <- c(8.59, 8.64, 7.43, 7.21, 6.39, 6.87, 7.89,
         9.79, 6.85, 7.54, 7.00, 8.80, 9.30, 8.03,
         8.65, 6.99, 8.40, 9.66, 7.14, 7.62, 7.44,
         8.55, 8.70, 9.14, 7.33, 8.58, 9.88, 9.94)

rat <- c(rep("Space",14), rep("Earth",14))

data <- tibble(rat, rbc)

# summary statistics
# (to help with hand calculations)
data %>% group_by(rat) %>% summarize(mean = mean(rbc), stdev = sd(rbc))

# t-test for two independent groups
t.test(rbc ~ rat, data = data, alternative = "two")
# left side of the ~ is the outcome of interest
# right side of the ~ is the grouping variable
# alternative = "two" specifies a two-tailed test
# can specify "greater" for a right-tail test
# or "less" for a left-tail test

# note that if you want a two-sided CI, you must specify a two-tailed test
# (if I want a one-tailed test, I will have a separate t.test() call for the CI)

# change the conf.level for different alpha
t.test(rbc ~ rat, data = data, alternative = "two", conf.level = 0.99)

# check the variance assumption (see 11-4 two-var.R for more details)
var.test(rbc ~ rat, data = data, alternative = "two.sided")
