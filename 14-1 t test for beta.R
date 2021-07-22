library(tidyverse)

# create data
age <- c(25, 25, 28, 32, 32, 32, 38, 42, 48, 51, 51, 58, 62, 65)
chol <- c(180, 195, 186, 180, 210, 197, 239, 183, 204, 221, 243, 208, 228, 269)
data <- tibble(age, chol)

# the following section is to help with hand calculations
###########################################
mean(age)
sd(age)
mean(chol)
sd(chol)

# numerator of Pearson's correlation
data$num_x <- ((data$age-mean(age))/sd(age))
data$num_y <- ((data$chol-mean(chol))/sd(chol))
data$num <- data$num_x*data$num_y

# calculate Pearson's 
r <- sum(data$num)/(13)

# find slope for regression line -- reuse Pearson's
b1 <- r*(sd(chol)/sd(age))

# find intercept for regression line
b0 <- mean(chol) - b1*mean(age)

# sum of squared resid for s_e
m1 <- lm(chol ~ age, data = data)
data$resid <- residuals(m1)
sum(data$resid^2)

# s_e
s_e <- sqrt(sum(data$resid^2/12))

# s_b1
s_b1 <- s_e/(sd(age)*sqrt(13))

# t
b1/s_b1
###########################################

# model of interest
m1 <- lm(chol ~ age, data = data)

# view results through the summary() function
summary(m1)
