# --- Install and load necessary packages ---
# Uncomment these lines if packages aren't installed
# install.packages("BDgraph")
# install.packages("mvtnorm")

library(BDgraph)
library(mvtnorm)

# --- Step 1: Simulate latent multivariate Gaussian data ---
set.seed(123)
p <- 5           # number of variables
n <- 200         # sample size

# True precision matrix with some zero entries (i.e., conditional independencies)
Omega_true <- matrix(0, p, p)
diag(Omega_true) <- 1
Omega_true[1, 2] <- Omega_true[2, 1] <- 0.5
Omega_true[2, 3] <- Omega_true[3, 2] <- 0.4
Omega_true[4, 5] <- Omega_true[5, 4] <- 0.6

Sigma_true <- solve(Omega_true)  # True covariance matrix
Z <- rmvnorm(n, sigma = Sigma_true)

# --- Step 2: Generate observed mixed data (copula idea) ---
data <- matrix(NA, n, p)
colnames(data) <- c("Hippocampus", "Education", "Sex", "Cognition", "Metabolism")

# Continuous variables
data[,1] <- Z[,1]
data[,4] <- Z[,4]
data[,5] <- Z[,5]

# Ordinal variable (Education: 0–3)
data[,2] <- cut(Z[,2], breaks = quantile(Z[,2], probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = FALSE) - 1

# Binary variable (Sex)
data[,3] <- ifelse(Z[,3] > 0, 1, 0)

# Convert to data frame
data <- as.data.frame(data)

# --- Step 3: Fit the Gaussian Copula Graphical Model (GCGM) ---
result <- bdgraph(data = data, method = "gcgm", iter = 10000, save = TRUE)

# --- Step 4: Posterior Graph Inference ---
# Plot most probable graph structure
plot(result, main = "Estimated Graph Structure (BDgraph)")

# Print summary of edge inclusion probabilities
summary(result)
