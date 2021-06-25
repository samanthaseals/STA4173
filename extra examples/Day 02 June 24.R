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

