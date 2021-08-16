##### EXAMPLE 1 #####

# enter counts in a matrix
observed_table <- matrix(c(600, 63, 112, 144,
                           720, 142, 355, 459,
                           93, 51, 119, 127), 
                         nrow = 3, ncol = 4, byrow = T)
# I prefer to include breaks to make it look like the table given
# just for checking purposes
# make sure you edit the number of rows (nrow) and columns (ncol)

# give names to the rows and columns
rownames(observed_table) <- c("Very Happy", "Pretty Happy", "Not Too Happy")
colnames(observed_table) <- c("Married", "Widowed", "Divorced/Separated", "Never Married")

# print table to make sure it is what we want
observed_table

# chi-squared test for independence
chisq.test(observed_table)

##### EXAMPLE 2 #####

# enter counts in a matrix
observed_table <- matrix(c(51, 5, 16,
                           1532, 152, 163), 
                         nrow = 2, ncol = 3, byrow = T)
# I prefer to include breaks to make it look like the table given
# just for checking purposes
# make sure you edit the number of rows (nrow) and columns (ncol)

# give names to the rows and columns
rownames(observed_table) <- c("Pain", "No Pain")
colnames(observed_table) <- c("Zocor", "Placebo", "Cholestramine")

# print table to make sure it is what we want
observed_table

# chi-squared test for independence
chisq.test(observed_table)