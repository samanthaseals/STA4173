##### EXAMPLE 1 #####

# z test for two independent proportions
prop.test(x = c(547, 368), n = c(2103, 1671), alternative = "greater")
# x is the vector of counts with the characteristic of interest
# n is the vector of sample sizes
# NOTE that they must be given in the corresponding order
# e.g., we are looking at 547/2103 and 368/1671
# alternative = "greater" specifies a right-tailed test

# need to specify alternative = "two.sided" for proper CI
prop.test(x = c(547, 368), n = c(2103, 1671), alternative = "two.sided")
# all of the above is the same *except*
# we are specifying alternative = "two.sided"
# so that we get a two-sided CI
# (compare to the first prop.test() -- it gives a one-sided CI)

##### EXAMPLE 2 #####

# z test for two independent proportions
prop.test(x = c(59, 56), n = c(220, 175), alternative = "two.sided")
# x is the vector of counts with the characteristic of interest
# n is the vector of sample sizes
# NOTE that they must be given in the corresponding order
# e.g., we are looking at 59/220 and 56/175
# alternative = "two.sided" specifies a two-tailed test