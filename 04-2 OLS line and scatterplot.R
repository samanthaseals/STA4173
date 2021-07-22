library(tidyverse)

# create data
x <- c(100, 102, 103, 101, 105, 100, 99, 105)
y <- c(257, 264, 274, 266, 277, 263, 258, 275)
data <- tibble(x, y)

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
r <- sum(data$num)/(7)

# find slope for regression line -- reuse Pearson's
b1 <- r*(sd(y)/sd(x))

# find intercept for regression line
b0 <- mean(y) - b1*mean(x)
###########################################

# model of interest
m1 <- lm(y ~ x, data = data)
# the outcome is on the left of the ~
# the predictor is on the right of the ~

# summary function to get results
summary(m1)

# get regression line for plotting purposes
# this creates a new variable (called "pred")
# that is the predicted value, or y hat
data$pred <- predict(m1)

# create scatterplot with geom_point(y = observed value)
# overlay regression line with geom_line(y = predicted value)
ggplot(data = data, aes(x = x, y = y)) +
  geom_point(size=5) +
  geom_line(aes(y = pred)) +
  ylab("Distance (yards)") +
  xlab("Club-Head Speed (mph)") +
  theme_minimal() +
  theme(text = element_text(size=20))

#ggsave("/Users/sseals/Desktop/L63fig1.png")