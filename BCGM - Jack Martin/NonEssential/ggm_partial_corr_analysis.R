#Import Libraries
library(mvtnorm)
library(glasso)
library(qgraph)

set.seed(42)
n <- 200       # number of samples
p <- 6         # number of variables

# Step 1: Define the true precision matrix (sparse)
Theta <- matrix(0, p, p)
Theta[1,2] <- Theta[2,1] <- 0.6
Theta[2,3] <- Theta[3,2] <- -0.4
Theta[3,4] <- Theta[4,3] <- 0.5
Theta[5,6] <- Theta[6,5] <- -0.3
diag(Theta) <- 1

# Step 2: Generate multivariate normal data
Sigma <- solve(Theta)  # Covariance matrix is inverse of precision
data <- rmvnorm(n, mean = rep(0, p), sigma = Sigma)

# Step 3: Standardize the data
data_scaled <- scale(data)

# Step 4: Estimate the graphical model using graphical lasso
S <- cov(data_scaled)  # Empirical covariance
glasso_fit <- glasso(S, rho = 0.1)  # rho is the regularization parameter

# Step 5: Extract and visualize the graph
adjacency <- abs(glasso_fit$wi) > 1e-4  # precision matrix thresholded
diag(adjacency) <- 0  # remove self-loops
colnames(adjacency) <- rownames(adjacency) <- paste0("X", 1:p)

# Visualize using qgraph
qgraph(adjacency, layout = "spring", vsize = 8,
       title = "Estimated Gaussian Graphical Model (R)",
       labels = colnames(adjacency))


Theta_hat <- glasso_fit$wi  # estimated precision matrix
D <- sqrt(diag(Theta_hat))
partial_corr <- -Theta_hat / (D %*% t(D))
diag(partial_corr) <- 1  # ensure diagonal is 1

round(partial_corr, 2)  # display partial correlations

# Force symmetry
partial_corr_sym <- (partial_corr + t(partial_corr)) / 2

# Plot the network
qgraph(partial_corr_sym, graph = "cor", layout = "spring")

# Plot and store graph object
graph_obj <- qgraph(partial_corr_sym, graph = "cor", layout = "spring")

# Use core centrality function
centrality <- centrality(graph_obj)

# View centrality metrics
print(centrality)
centralityPlot(centrality)


barplot(centrality$OutDegree,
        names.arg = paste0("X", 1:6),
        col = "steelblue",
        main = "Node Strength (OutDegree)",
        ylab = "Strength")



