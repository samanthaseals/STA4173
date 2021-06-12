##### EXAMPLE 1 #####

prop.test(x = 66, n = 705, p = 0.08, alternative = "greater", correct = TRUE)

# must specify alternative = "two.sided" for a proper CI
prop.test(x = 66, n = 705, p = 0.08, alternative = "two.sided", correct = TRUE)

##### EXAMPLE 2 ######

prop.test(x = 408, n = 1500, p = 0.24, alternative = "greater", correct = TRUE)

prop.test(x = 408, n = 1500, p = 0.24, alternative = "two.sided", correct = TRUE)