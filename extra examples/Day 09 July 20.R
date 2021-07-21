library(tidyverse)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

##### PROBLEM 1 #####

fico <- c(545, 595, 640, 675, 705, 750)
interest <- c(18.982, 17.967, 12.218, 8.612, 6.680, 5.150)
data <- tibble(fico, interest)

# the following section is to help with hand calculations
###########################################
mean(interest)
sd(interest)
mean(fico)
sd(fico)

# numerator of Pearson's correlation
data$num_x <- ((data$fico-mean(fico))/sd(fico))
data$num_y <- ((data$interest-mean(interest))/sd(interest))
data$num <- data$num_x*data$num_y

# calculate Pearson's 
r <- sum(data$num)/(5)

# find slope for regression line -- reuse Pearson's
b1 <- r*(sd(interest)/sd(fico))

# find intercept for regression line
b0 <- mean(interest) - b1*mean(fico)
###########################################

# scatterplot
ggplot(data, aes(x=fico, y=interest)) +
  geom_point() +
  theme_minimal() +
  labs(x = "FICO Score", y = "Interest Rate")

# Pearson's correlation
cor(data)

# Spearman's correlation
cor(data, method = "spearman")

# model of interest
m1 <- lm(interest ~ fico, data = data)

# look at summary to get model
summary(m1)

# confidence interval for beta
confint(m1)

# assessment graph
almost_sas(m1)

##### PROBLEM 2 #####

length <- c(139, 138, 139, 120.5, 149, 141, 141, 150, 166, 151.5, 129.5, 150)
weight <- c(110, 60, 90, 60, 85, 100, 95, 85, 155, 140, 105, 110)
data <- tibble(length, weight)

# the following section is to help with hand calculations
###########################################
mean(length)
sd(length)
mean(weight)
sd(weight)

# numerator of Pearson's correlation
data$num_x <- ((data$length-mean(length))/sd(length))
data$num_y <- ((data$weight-mean(weight))/sd(weight))
data$num <- data$num_x*data$num_y

# calculate Pearson's 
r <- sum(data$num)/(11)

# find slope for regression line -- reuse Pearson's
b1 <- r*(sd(weight)/sd(length))

# find intercept for regression line
b0 <- mean(weight) - b1*mean(length)
###########################################

# scatterplot
ggplot(data, aes(x=length, y=weight)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Total Length", y = "Weight")

# Pearson's correlation
cor(data)

# Spearman's correlation
cor(data, method = "spearman")

# model of interest
m1 <- lm(weight ~ length, data = data)

# look at summary to get model
summary(m1)

# confidence interval for beta
confint(m1)

# assessment graph
almost_sas(m1)
