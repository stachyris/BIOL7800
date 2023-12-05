# Lets run all of the statistical tests and visualization in the base R
library(dplyr)
# First let's see what's happening with Orders
order_table <- base::table(df$order) # creating a table
order_chi_square <- chisq.test(order_table) # performing chi-square test
# checking the results

order_residuals <- resid(order_chi_square) # running residuals to check the representation
 # checking the results

#par(mar = c(9, 6, 4, 2) + 0.1)
barplot(order_residuals, names.arg = levels(df$Order), main = "Standardized Residuals - Orders", col = "#11A797", las = 2) # plotting the results


# Let's see what's happening with Families of birds
family_table <- table(df$family) # creating a table
family_chi_square <- chisq.test(family_table) # perfomning Chi-square test
 # checking the results. 

family_residuals <- resid(family_chi_square) # running resuduals to check the represntation. 
 # checking the results

par(mar = c(9, 6, 4, 2) + 0.1)
barplot(family_residuals, names.arg = levels(df$family), main = "Standardized Residuals - Families", col = "#11A797", las = 2) # plotting the results. 
barplot(order_residuals, names.arg = levels(df$order), main = "Standardized Residuals - order", col = "#11A797", las = 2) # plotting the results. 





