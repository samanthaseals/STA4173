library(tidyverse)
library(ggpubr)

###### PROBLEM 1 ######

# create dataset
speed <- c(28, 48, 56, 38, 31, 25, 43, 46, 50, 35, 55, 40, 42, 26, 47,
           24, 26, 42, 34, 37, 31, 47, 38, 17, 29, 23, 40, 37, 52, 41)
meter <- c(rep("on", 15), rep("off", 15))
data <- tibble(meter, speed)

# group means and standard deviations
data %>% group_by(meter) %>% summarize(mean = mean(speed), sd = sd(speed))

# folded F
var.test(speed ~ meter, data=data, alternative = "two.sided")

# create QQ plots
on <- data %>% filter(meter=="on") 
meter_on <- ggplot(on, aes(sample = speed)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Meter On") +
  theme(text = element_text(size=20))

off <- data %>% filter(meter=="off")
meter_off <- ggplot(off, aes(sample = speed)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Meter Off") +
  theme(text = element_text(size=20))

both <- ggarrange(meter_on, meter_off, ncol=2, nrow=1)
both
#ggsave("/Users/sseals/Desktop/D02fig2.png")

# two-sample t-test
t.test(speed ~ meter, data = data, alternative = "less")

# confidence interval
t.test(speed ~ meter, data = data, alpha=0.10)

###### PROBLEM 2 ######

suv <- c(1721, 1434,  850, 2329, 1415, 1470, 2884)
car <- c(1274, 2327, 3223, 2058, 3095, 3386, 4560)

data <- tibble(suv, car)

# differences and summary statistics
data$diff <- data$suv - data$car
mean(data$diff)
sd(data$diff)

# paired t-test
t.test(suv, car, paired = TRUE, alternative = "less")

# QQ plot
p <- ggplot(data, aes(sample = diff)) +
  stat_qq(size=3) +
  stat_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  theme(text = element_text(size=20))
p

#ggsave("/Users/sseals/Desktop/D02fig4.png")

##### PROBLEM 3 #####

rating <- c("E", "G", "F", "F", "P", 
            "E", "G", "G", "F", "F", 
            "F", "G", "E", "P", "F",
            "F", "F", "G", "G", "F", 
            "F", "P", "F", "G", "G", 
            "E", "F", "G", "F", "P")

# create a numeric version
rating2 <- if_else(rating == "E", 1, 
                   if_else(rating == "G", 2,
                           if_else(rating == "F", 3, 4)))

critic <- c(rep("A", 15), rep("B", 15))

data <- tibble(critic, rating, rating2)

data$rank <- rank(data$rating2, ties.method = "average")

sums <- data %>% 
  group_by(critic) %>%
  summarize_at(vars(rank),
               list(name = sum))

wilcox.test(data$rating2~data$critic, exact = FALSE, alternative="two.sided")

##### PROBLEM 4 #####

avis <- c(67.54, 97.16, 103.43, 38.79, 87.90, 109.86, 109.35, 82.86, 112.65, 116.46, 105.49)
hertz <- c(59.14, 97.15, 103.14, 36.63, 87.80, 110.09, 109.35, 83.03, 113.32, 114.53, 107.04)

data <- tibble(avis, hertz)

data$diff <- data$avis - data$hertz
data$absdiff <- abs(data$diff)

# remove diff=0
data <- data %>% filter(diff != 0)

data$rank <- rank(data$absdiff, ties.method = "average")
data$rank <- if_else(data$diff<0, data$rank*(-1), data$rank)

data$sign <- if_else(data$diff<0, "-", "+")

data %>% 
  group_by(sign) %>%
  summarize_at(vars(rank),
               list(name = sum))

wilcox.test(data$avis, data$hertz, paired = TRUE, alternative = "less", exact = FALSE)

