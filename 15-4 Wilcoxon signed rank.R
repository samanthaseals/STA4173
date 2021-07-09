library(tidyverse)
library(ggpubr)

##### EXAMPLE 1 #####

# create data
day <- seq(1,14,1)
WBA <- c(8.9, 6.3, 6.2, 7.2, 2.8, 3.3, 23.6, 6.0, 15.6, 5.2, 6.3, 10.1, 4.0, 8.4)
MCD <- c(8.5, 7.6, 8.3, 10.4, 2.5, 2.6, 3.5, 4.7, 9.0, 6.0, 5.6, 5.0, 4.4, 5.6)

data <- tibble(day, WBA, MCD)

# create difference
data$diff <- data$WBA - data$MCD

# create QQ plot
p <- ggplot(data=data, aes(sample=diff)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("WBA - MCD")+
  theme_minimal()

# display the image
p

# save the resulting image
#ggsave("/Users/sseals/Desktop/L73fig1.png")


# the following section is to help with hand calculations
###########################################
# find the absolute difference
data$absdiff <- abs(data$diff)

# remove diff=0
data <- data %>% filter(diff != 0)

# find the ranks
data$rank <- rank(data$absdiff, ties.method = "average")

# keep the sign on the rank
data$rank <- if_else(data$diff<0, data$rank*(-1), data$rank)

# create a variable with the sign so that we can find the sum of each
data$sign <- if_else(data$diff<0, "-", "+")

# find the sum of the ranks by group
data %>% 
  group_by(sign) %>%
  summarize_at(vars(rank),
               list(name = sum))
###########################################

# perform the signed rank test
wilcox.test(data$WBA, data$MCD, paired = TRUE, alternative = "less", exact = FALSE)

# first two arguments are the variables of interest
# paired = TRUE tells it to do the Wilcoxon signed rank (without it will do the Mann-Whitney)
# alternative = "less" tells it that the alternative is first var < second var
# exact = FALSE gets rid of an issue computing p-values when the data has ties

##### EXAMPLE 2 #####

# create the data
child <- seq(1,7,1)
reported <- c(68, 71, 63, 70, 71, 60, 59.0)
measured <- c(67.9, 69.9, 64.9, 68.3, 70.3, 60.6, 59.2)

data <- tibble(child, reported, measured)

# find the difference
data$diff <- data$reported - data$measured

# create the QQ plot
p <- ggplot(data=data, aes(sample=diff)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Reported - Measured")+
  theme_minimal()

# display the image
p

# save the image
#ggsave("/Users/sseals/Desktop/L73fig3.png")

# the following section is to help with hand calculations
###########################################
# find the absolute difference
data$absdiff <- abs(data$diff)

# remove diff=0
data <- data %>% filter(diff != 0)

# find the ties
data$rank <- rank(data$absdiff, ties.method = "average")

# keep the sign on the rank
data$rank <- if_else(data$diff<0, data$rank*(-1), data$rank)

# create a variable with the sign so that we can find the sum of each
data$sign <- if_else(data$diff<0, "-", "+")

# find the sum of the ranks by group
data %>% 
  group_by(sign) %>%
  summarize_at(vars(rank),
               list(name = sum))
###########################################

# perform the signed rank test
wilcox.test(data$measured, data$reported, paired = TRUE, alternative = "two.sided", exact = FALSE)
# now alternative = "two.sided" tests the alternative of first var does not equal second var