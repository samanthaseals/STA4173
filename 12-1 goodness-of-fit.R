##### EXAMPLE 1 #####

# create vector of counts
counts <- c(161, 144, 138, 184, 247, 188, 217, 105, 116)
probs <- c(0.099, 0.098, 0.093, 0.135, 0.179, 0.131, 0.149, 0.061, 0.055)

# chi-squared goodness-of-fit
chisq.test(counts, p = probs)
# the first argument is the vector of counts
# p = [vector of probabilities]

##### EXAMPLE 2 #####

# create vector of counts
counts <- c(46, 76, 83, 81, 81, 80, 53)
probs <- c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7)

# chi-squared goodness-of-fit
chisq.test(counts, p = probs)
# the first argument is the vector of counts
# p = [vector of probabilities]