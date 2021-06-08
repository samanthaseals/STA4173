library(tidyverse)
library(multcomp)
library(broom)

strength <- c(15.4, 12.9, 17.2, 16.6, 19.3,
              17.2, 14.3, 17.6, 21.6, 17.5,
              5.5,  7.7, 12.2, 11.4, 16.4,
              11.0, 12.4, 13.5,  8.9,  8.1)

system <- c(rep("Cojet",5), rep("Silistor",5), rep("Cimara",5), rep("Ceramic",5))

data <- tibble(system, strength)

data <- data %>% mutate (system = factor(system))

# METHOD 1
m1 <- lm(strength ~ system, data=data)

summary(glht(m1, linfct = mcp(system = "Tukey")))
ci1 <- confint(glht(m1, linfct = mcp(system = "Tukey")))
plot(ci1)

# METHOD 2
m1 <- aov(data=data, strength ~ system)
tky = as.data.frame(TukeyHSD(m1)$system)
tky$pair = rownames(tky)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.05, 1), 
                           label=c("p < 0.05", "p ≥ 0.05")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
  geom_point(aes(pair, diff)) +
  xlab("Parwise Comparison") +
  ylab("Difference") +
  labs(colour="") +
  theme_minimal()
