##### EXAMPLE 1 #####

# enter counts in a matrix
observed_table <- matrix(c(293, 43,
                           31, 103), 
                         nrow = 2, ncol = 2, byrow = T)
# I prefer to include breaks to make it look like the table given
# just for checking purposes

# give names to the rows and columns
rownames(observed_table) <- c("B Healed", "B Did Not Heal")
colnames(observed_table) <- c("A Healed", "A Did Not Heal")

# print table to make sure it is what we want
observed_table

# McNemar's test
mcnemar.test(observed_table, correct = FALSE)
# we include correct = FALSE because we do not want the continuity correction

##### EXAMPLE 2 #####

# enter counts in a matrix
observed_table <- matrix(c(494, 335,
                           126, 537), 
                         nrow = 2, ncol = 2, byrow = T)
# I prefer to include breaks to make it look like the table given
# just for checking purposes

# give names to the rows and columns
rownames(observed_table) <- c("Tap - Agree", "Tap - Disagree")
colnames(observed_table) <- c("Stop - Agree", "Stop - Disagree")

# print table to make sure it is what we want
observed_table

# McNemar's test
mcnemar.test(observed_table, correct = FALSE)
# we include correct = FALSE because we do not want the continuity correction