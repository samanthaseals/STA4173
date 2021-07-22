##### EXAMPLE 1 #####

# z test for a single proportion
prop.test(x = 66, n = 705, p = 0.08, alternative = "greater", correct = TRUE)
# x is the count with the characteristic of interest
# n is the total sample size
# p is the hypothesized value
# alternative = "greater" specifies a right-tailed test

# must specify alternative = "two.sided" for a proper CI
prop.test(x = 66, n = 705, p = 0.08, alternative = "two.sided", correct = TRUE)
# all of the above is the same *except*
# we are specifying alternative = "two.sided"
# so that we get a two-sided CI
# (compare to the first prop.test() -- it gives a one-sided CI)

##### EXAMPLE 2 ######

# z test for a single proportion
prop.test(x = 408, n = 1500, p = 0.24, alternative = "greater", correct = TRUE)
# x is the count with the characteristic of interest
# n is the total sample size
# p is the hypothesized value
# alternative = "greater" specifies a right-tailed test

# must specify alternative = "two.sided" for a proper CI
prop.test(x = 408, n = 1500, p = 0.24, alternative = "two.sided", correct = TRUE)
# all of the above is the same *except*
# we are specifying alternative = "two.sided"
# so that we get a two-sided CI
# (compare to the first prop.test() -- it gives a one-sided CI)