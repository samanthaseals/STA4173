library(tidyverse)
library(car)
library(lindia)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

##### PROBLEM 1 #####

sex <- c(rep("Female", 10), rep("Male", 10))
MRI <- c(816932, 951545, 991305, 833868, 856472, 852244, 790619, 866662, 857782, 948066,
         949395, 1001121, 1038437, 965353, 955466, 1079549, 924059, 955003, 935494, 949589)
IQ <- c(133, 137, 138, 132, 140, 132, 135, 130, 133, 133,
        140, 140, 139, 133, 133, 141, 135, 139, 141, 144)

data <- tibble(sex, MRI, IQ)

# model of interest
m1 <- lm(IQ ~ sex + MRI)
summary(m1)

# confidence intervals
confint(m1)

# check for outliers
data$outlier <- abs(rstandard(m1))>2.5
data %>% count(outlier)

# check for leverage/influence
gg_cooksd(m1) + 
  geom_point(size=3) + 
  theme_minimal() + 
  theme(text = element_text(size=20))

# check multicollinearity
vif(m1)

# check assumptions
almost_sas(m1)

##### PROBLEM 2 #####

experience <- c(21, 14, 4, 16, 12, 20, 25, 8, 24, 28, 4, 15)
education <- c(6, 3, 8, 8, 4, 4, 1, 3, 12, 9, 11, 4)
income <- c(34.7, 17.9, 22.7, 63.1, 33.0, 41.4, 20.7, 14.6, 97.3, 72.1, 49.1, 52.0)

data <- tibble(experience, education, income)

# correlation matrix
cor(data)

# model of interest
m2 <- lm(income ~ experience + education + experience:education)
summary(m2)

# model without interaction
m3 <- lm(income ~ experience + education)
summary(m3)

# confidence intervals
confint(m3)

# check for outliers
data$outlier <- abs(rstandard(m3))>2.5
data %>% count(outlier)
data$outlier

# check for leverage/influence
gg_cooksd(m3) + 
  geom_point(size=3) + 
  theme_minimal() + 
  theme(text = element_text(size=20))

# check multicollinearity
vif(m3)

# check assumptions
almost_sas(m3)
