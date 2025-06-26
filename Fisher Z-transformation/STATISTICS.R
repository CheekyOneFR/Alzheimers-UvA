########################
# PARTIAL CORRELATIONS #
########################


# Load required packages
#install.packages(c("ppcor", "reshape2", "ggplot2"))
library(ppcor)

data_filtered <- data_3[, !(colnames(data_3) %in% "diagnosis")]

# Subset numeric variables
numeric_data <- data_filtered[sapply(data_filtered, is.numeric)]

# Compute partial correlation matrix
pcor_result <- pcor(numeric_data)
partial_corr_matrix <- pcor_result$estimate


#######################################################
## Fisher z-transformation ##
#######################################################


# Make a copy of the previous matrix to avoid modifying original
z_matrix <- partial_corr_matrix

# Apply Fisher z-transformation element-wise
# Avoid applying to perfect correlations (1 or -1) — replace with Inf / NA if needed
z_matrix <- apply(z_matrix, c(1, 2), function(r) {
  if (abs(r) >= 1) return(NA)  # or Inf depending on your needs
  0.5 * log((1 + r) / (1 - r))
})

# Optional: Round for readability
z_matrix <- round(z_matrix, 3)

# View result
print(z_matrix)


######################################################################################################
# 95% confidence intervals for all sample partial correlations, 
# based on the Fisher Z-transformation, and use these to decide whether to reject the null hypothesis
######################################################################################################

#Prepare output dataframe
results <- data.frame(
  Var1 = character(), Var2 = character(),
  r = numeric(), CI_lower = numeric(), CI_upper = numeric(),
  significant = logical(), stringsAsFactors = FALSE
)

alpha <- 0.05
z_crit <- qnorm(1 - alpha/2)
n_var <- ncol(partial_corr_matrix)
n_obs <- nrow(data_3)  # total number of samples
se <- 1 / sqrt(n_obs - n_var - 1)  # standard error of Fisher z

# Loop through upper triangle of correlation matrix
for (i in 1:(n_var - 1)) {
  for (j in (i + 1):n_var) {
    r <- partial_corr_matrix[i, j]
    if (!is.na(r)) {
      # Fisher z-transform
      z <- 0.5 * log((1 + r) / (1 - r))
      
      # Confidence interval in z-space
      z_low <- z - z_crit * se
      z_up  <- z + z_crit * se
      
      # Back-transform to r-space
      r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
      r_up  <- (exp(2 * z_up)  - 1) / (exp(2 * z_up)  + 1)
      
      # Check if zero is outside the CI
      is_sig <- !(0 >= r_low & 0 <= r_up)
      
      # Store result
      results <- rbind(results, data.frame(
        Var1 = rownames(partial_corr_matrix)[i],
        Var2 = colnames(partial_corr_matrix)[j],
        r = r, CI_lower = r_low, CI_upper = r_up, significant = is_sig
      ))
    }
  }
}

# View significant results, based on CI exclusion of 0,
significant_results <- results[results$significant == TRUE, ]
print(significant_results)





