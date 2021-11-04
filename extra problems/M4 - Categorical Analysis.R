#### PROBLEM 1 ####
# 95% CI; can use for both CI and HT
prop.test(x = c(39, 46), n = c(50, 50), alternative = "two.sided", correct = FALSE)

#### PROBLEM 2 ####
# 99% CI; can use for both CI and HT
prop.test(x = 200, n = 1200, p = 0.15, alternative = "two.sided", conf.level = 0.99, correct = FALSE)

#### PROBLEM 3 ####
# 90% CI - need two sided for correct CI
prop.test(x = 5, n = 40, p = 0.1, alternative = "two.sided", conf.level = 0.9, correct = FALSE)

# need one sided test
prop.test(x = 5, n = 40, p = 0.1, alternative = "greater", conf.level = 0.9, correct = FALSE)

#### PROBLEM 4 ####
# 95% CI - needed two sided for correct CI
prop.test(x = c(15, 10), n = c(50, 50), alternative = "two.sided", correct = FALSE)

# need one sided test
prop.test(x = c(15, 10), n = c(50, 50), alternative = "greater", correct = FALSE)

#### PROBLEM 5 ####
counts <- c(273, 263, 211, 206)
probs <- c(0.25, 0.25, 0.25, 0.25)
chisq.test(counts, p = probs)

#### PROBLEM 6 ####
counts <- c(7, 16, 10, 13, 6, 12)
probs <- c(0.133, 0.191, 0.246, 0.104, 0.114, 0.212)
chisq.test(counts, p = probs)

#### PROBLEM 7 ####
observed_table <- matrix(c(178, 88, 208,
                           137, 69, 143,
                           44, 35, 44,
                           34, 33, 51), 
                         nrow = 4, ncol = 3, byrow = T)

rownames(observed_table) <- c("< 12", "12", "13-15", ">16")
colnames(observed_table) <- c("Current", "Former", "Never")

chisq.test(observed_table)

#### PROBLEM 8 ####
observed_table <- matrix(c(220, 144, 62,
                           279, 410, 351), 
                         nrow = 2, ncol = 3, byrow = T)

rownames(observed_table) <- c("Positive", "Negative")
colnames(observed_table) <- c("Democrat", "Independent", "Republican")

chisq.test(observed_table)

#### PROBLEM 9 ####
observed_table <- matrix(c(67, 448,
                           327, 2187), 
                         nrow = 2, ncol = 2, byrow = T)

rownames(observed_table) <- c("B Smoker", "B Non-Smoker")
colnames(observed_table) <- c("A No Seat Belt", "A Seat Belt")

mcnemar.test(observed_table, correct = FALSE)

#### PROBLEM 10 ####
observed_table <- matrix(c(176, 69,
                           58, 17), 
                         nrow = 2, ncol = 2, byrow = T)

rownames(observed_table) <- c("B Gun Favor", "B Gun Oppose")
colnames(observed_table) <- c("A Penalty Favor", "A Penalty Oppose")

mcnemar.test(observed_table, correct = FALSE)
