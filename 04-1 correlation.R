# create data
x <- c(100, 102, 103, 101, 105, 100, 99, 105)
y <- c(257, 264, 274, 266, 277, 263, 258, 275)

# the following section is to help with hand calculations
###########################################
mean(x)
mean(y)
sqrt(var(x))
sqrt(var(y))

data$num_x <- ((data$x-mean(x))/sd(x))
data$num_y <- ((data$y-mean(y))/sd(y))
data$num <- data$num_x*data$num_y

# calculate Pearson's 
sum(data$num)/(7)
###########################################

# request correlation between two values
cor(x, y)

# if data is in a tibble, can just plug in tibble name
# for the correlation matrix
# (if you have more than 2 variables, it will give
# all pairwise correlations)
data <- tibble(x, y)
cor(data)
