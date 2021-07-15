library(tidyverse)
library(multcomp)

# create data
weight <- c(157.78, 136.79, 138.84,
            139.72, 125.47, 117.14,
            129.35, 110.73, 118.38,
            137.07, 146.28, 130.27,
            117.46, 128.54,  99.16,
            97.43, 125.26, 115.42)

age <- c(rep("Aged 20 - 29", 9), rep("Aged 30 - 39", 9))

diet <- c(rep(c(rep("Diet 1", 3), rep("Diet 2", 3), rep("Diet 3", 3)),2))

data <- tibble(age, diet, weight)
data <- data %>% mutate (diet = factor(diet))

# the following section is to help with hand calculations
###########################################
means <- data %>% 
  group_by(diet) %>% 
    summarize(mean = mean(weight))
###########################################

# METHOD 1
# create model
m1 <- anova(lm(weight ~ age + diet, data = data))

# request Tukey's comparison
summary(glht(m1, linfct = mcp(diet = "Tukey")))
ci1 <- confint(glht(m1, linfct = mcp(system = "Tukey")))
plot(ci1)

# METHOD 2

# create model
m1 <- aov(data=data, weight ~ age + diet)

# request Tukey's comparison
tky = as.data.frame(TukeyHSD(m1)$diet)
tky$pair = rownames(tky)

# plot pairwise TukeyHSD comparisons and color by significance level
# you do not need to tweak any of this code in the future as long
# as you follow what is done above (i.e., naming the dataset needed "tky")
ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.05, 1), 
                           label=c("p < 0.05", "p ≥ 0.05")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
  geom_point(aes(pair, diff)) +
  xlab("Parwise Comparison") +
  ylab("Difference") +
  labs(colour="") +
  theme_minimal() +
  theme(text = element_text(size=20))

# save graph
#ggsave("/Users/sseals/Desktop/L87fig2.png")