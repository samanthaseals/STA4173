##### EXAMPLE 1 #####

prop.test(x = c(547, 368), n = c(2103, 1671), alternative = "greater")

# need to specify alternative = "two.sided" for proper CI
prop.test(x = c(547, 368), n = c(2103, 1671), alternative = "two.sided")

##### EXAMPLE 2 #####

prop.test(x = c(59, 56), n = c(220, 175), alternative = "two.sided")
