install.packages("dplyr")
install.packages("huge")
install.packages("corrplot")
install.packages("ppcor")
install.packages("igraph")

library(dplyr)
library(huge)


df <- read.csv("~/Desktop/Thesis_data.csv", sep=";")

#Removing null value columns

df <- df %>% filter(is_na == 0)
df$is_na <- NULL

#First attempt: no diagnosis column

df_1 <- df %>% dplyr::select(-diagnosis)

#Making sure ordinal/categorical variables are stored as numeric values

df_1$Sex <- as.numeric(as.character(df_1$Sex))
df_1$APOE4 <- as.numeric(as.character(df_1$APOE4))
df_1$Amy_Stage <- as.numeric(as.character(df_1$Amy_Stage))

#Preparing the data matrix for huge.npn

df_1 <- df_1 %>% mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
df_1 <- df_1 %>% mutate(across(where(is.character), as.numeric))

###Pearson and partial correlation matrices###

pearson_corr <- cor(df_1, use = "pairwise.complete.obs")

library(corrplot)
dev.off()
plot.new()
corrplot(pearson_corr, method = "color", type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.cex = 0.6, tl.col = "black", number.cex = 0.7,
         main = "")
title("Pearson Correlation Matrix", line = 1)

library(ppcor)
pcor_res <- pcor(df_1)
partial_corr_raw <- pcor_res$estimate


diag(partial_corr_raw) <- 0
corrplot(partial_corr_raw, method = "color", type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.cex = 0.6, tl.col = "black", number.cex = 0.7,
         main = "")
title("Partial Correlation Matrix", line = 1)


#All standardization and transformations for glasso

df_1_npn <- huge.npn(as.matrix(df_1)) 

###EXTRA: check to see what npn does###

hist(df_1$Amy_Stage, main = "Before NPN", xlab = "Amy_Stage")
hist(df_1_npn[, "Amy_Stage"], main = "After NPN", xlab = "Amy_Stage")

summary(df_1$APOE4)
summary(df_1_npn[, "APOE4"])

######

#Glasso application and plotting

set.seed(1234)
glasso_fit_1 <- huge(df_1_npn, method = "glasso")

glasso_select_1 <- huge.select(glasso_fit_1, criterion = "stars")

plot(glasso_select_1, main = "Estimated Graph using StARS (no diagnosis column)")

#Lambda, precision matrix, adjacency matrix, sparsity level

optimal_lambda_1 <- glasso_select_1$opt.lambda
print(optimal_lambda_1)

precision_matrix_1 <- glasso_select_1$opt.icov
print(precision_matrix_1)

adj_matrix_1 <- glasso_select_1$refit
print(adj_matrix_1)

p_1 <- ncol(adj_matrix_1)
sparsity_level_1 <- sum(adj_matrix_1[upper.tri(adj_matrix_1)]) / (p_1 * (p_1 - 1) / 2)
print(paste("Sparsity level:", round(sparsity_level_1, 4)))

#Plotting the network graph including edge strengths

library(igraph)
node_labels_1 <- colnames(df_1)

# Binary adjacency matrix
adj_binary_1 <- (abs(glasso_select_1$opt.icov) > 1e-6) * 1
diag(adj_binary_1) <- 0
rownames(adj_binary_1) <- node_labels_1
colnames(adj_binary_1) <- node_labels_1

# Weighted adjacency matrix
partial_corr_mat <- -cov2cor(precision_matrix_1)
diag(partial_corr_mat) <- 0

adj_weighted_1 <- partial_corr_mat  
diag(adj_weighted_1) <- 0  

# Names for rows and columns
rownames(adj_weighted_1) <- node_labels_1
colnames(adj_weighted_1) <- node_labels_1
adj_weighted_1[adj_binary_1 == 0] <- 0

# Plotting binary network
g_bin_1 <- graph.adjacency(adj_binary_1, mode = "undirected", diag = FALSE)
V(g_bin_1)$name <- node_labels_1
plot(g_bin_1,
     vertex.label = V(g_bin_1)$name,
     vertex.label.cex = 0.5,
     vertex.size = 25,
     vertex.color = "skyblue",
     vertex.label.color = "black",
     edge.color = "gray60",
     main = "Binary network 1 (edge presence)")


# Plotting weighted network

g_wt_1 <- graph.adjacency(adj_weighted_1, mode = "undirected", weighted = TRUE, diag = FALSE)
V(g_wt_1)$name <- node_labels_1 

edges_1 <- get.edgelist(g_wt_1)
edge_weights_1 <- numeric(nrow(edges_1))
edge_colors_1 <- character(nrow(edges_1))

for (i in 1:nrow(edges_1)) {
  v1_1 <- edges_1[i, 1]
  v2_1 <- edges_1[i, 2]
  val_1 <- adj_weighted_1[v1_1, v2_1]  
  edge_weights_1[i] <- abs(val_1) * 25
  edge_colors_1[i] <- ifelse(val_1 > 0, "green",
                             ifelse(val_1 < 0, "red", "gray70"))
}

E(g_wt_1)$weight <- edge_weights_1
E(g_wt_1)$plot_weight <- pmax(E(g_wt_1)$weight, 0.5)
E(g_wt_1)$color <- edge_colors_1


layout_fr <- layout_with_fr(g_wt_1, 
                            niter = 1200, 
                            area = vcount(g_wt_1)^2.5, 
                            repulserad = vcount(g_wt_1)^4)

plot(g_wt_1,
     layout = layout_fr,
     edge.width = E(g_wt_1)$plot_weight,
     edge.color = edge_colors_1,
     vertex.label = V(g_wt_1)$name,
     vertex.label.cex = 0.7,
     vertex.size = 20,
     vertex.color = "skyblue",
     vertex.label.color = "black",
     main = "Weighted network (edge strengths)")

# Edge strengths - Partial correlations
prec_mat <- glasso_select_1$opt.icov
diag_prec <- diag(prec_mat)

partial_corr_mat <- -cov2cor(prec_mat)

diag(partial_corr_mat) <- 0

edge_list <- which(adj_binary_1 == 1, arr.ind = TRUE)

partial_corrs <- data.frame(
  from = rownames(adj_binary_1)[edge_list[, 1]],
  to = colnames(adj_binary_1)[edge_list[, 2]],
  partial_correlation_T1 = partial_corr_mat[edge_list]
)

partial_corrs <- partial_corrs[partial_corrs$from < partial_corrs$to, ]

partial_corrs <- partial_corrs %>% arrange(desc(abs(partial_correlation_T1)))

print(partial_corrs)

#Second attempt: include diagnosis column

df_2 <- df

#Making sure ordinal/categorical variables are stored as numeric values

df_2$Sex <- as.numeric(as.character(df_2$Sex))
df_2$APOE4 <- as.numeric(as.character(df_2$APOE4))
df_2$Amy_Stage <- as.numeric(as.character(df_2$Amy_Stage))
df_2$diagnosis <- as.numeric(as.character(df_2$diagnosis))

#Preparing the data matrix for huge.npn

df_2 <- df_2 %>% mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
df_2 <- df_2 %>% mutate(across(where(is.character), as.numeric))

#All standardization and transformations for glasso

df_2_npn <- huge.npn(as.matrix(df_2)) 

#Glasso application and plotting

set.seed(1234)
glasso_fit_2 <- huge(df_2_npn, method = "glasso")

glasso_select_2 <- huge.select(glasso_fit_2, criterion = "stars")

plot(glasso_select_2, main = "Estimated Graph using StARS (incl. diagnosis column)")

#Lambda, precision matrix, adjacency matrix, sparsity level

optimal_lambda_2 <- glasso_select_2$opt.lambda
print(optimal_lambda_2)

precision_matrix_2 <- glasso_select_2$opt.icov
print(precision_matrix_2)

adj_matrix_2 <- glasso_select_2$refit
print(adj_matrix_2)

p_2 <- ncol(adj_matrix_2)
sparsity_level_2 <- sum(adj_matrix_2[upper.tri(adj_matrix_2)]) / (p_2 * (p_2 - 1) / 2)
print(paste("Sparsity level:", round(sparsity_level_2, 4)))

#Plotting the network graph including edge strengths

library(igraph)
node_labels_2 <- colnames(df_2)

# Binary adjacency matrix
adj_binary_2 <- (abs(glasso_select_2$opt.icov) > 1e-6) * 1
diag(adj_binary_2) <- 0
rownames(adj_binary_2) <- node_labels_2
colnames(adj_binary_2) <- node_labels_2

# Weighted adjacency matrix
adj_weighted_2 <- -cov2cor(precision_matrix_2)
diag(adj_weighted_2) <- 0

rownames(adj_weighted_2) <- node_labels_2
colnames(adj_weighted_2) <- node_labels_2

adj_weighted_2[adj_binary_2 == 0] <- 0

# Plotting binary network
g_bin_2 <- graph.adjacency(adj_binary_2, mode = "undirected", diag = FALSE)
V(g_bin_2)$name <- node_labels_2
plot(g_bin_2,
     vertex.label = V(g_bin_2)$name,
     vertex.label.cex = 0.5,
     vertex.size = 25,
     vertex.color = "skyblue",
     vertex.label.color = "black",
     edge.color = "gray60",
     main = "Binary network 2 (edge presence)")


# Plotting weighted network

g_wt_2 <- graph.adjacency(adj_weighted_2, mode = "undirected", weighted = TRUE, diag = FALSE)
V(g_wt_2)$name <- node_labels_2 

edges_2 <- get.edgelist(g_wt_2)
edge_weights_2 <- numeric(nrow(edges_2))
edge_colors_2 <- character(nrow(edges_2))

for (i in 1:nrow(edges_2)) {
  v1_2 <- edges_2[i, 1]
  v2_2 <- edges_2[i, 2]
  val_2 <- adj_weighted_2[v1_2, v2_2]  
  edge_weights_2[i] <- abs(val_2) * 25
  edge_colors_2[i] <- ifelse(val_2 > 0, "green",
                             ifelse(val_2 < 0, "red", "gray70"))
}

E(g_wt_2)$weight <- edge_weights_2
E(g_wt_2)$plot_weight <- pmax(E(g_wt_2)$weight, 0.5)
E(g_wt_2)$color <- edge_colors_2

layout_fr <- layout_with_fr(g_wt_2, 
                            niter = 1300, 
                            area = vcount(g_wt_2)^3, 
                            repulserad = vcount(g_wt_2)^4)

plot(g_wt_2,
     layout = layout_fr,
     edge.width = E(g_wt_2)$plot_weight,
     edge.color = edge_colors_2,
     vertex.label = V(g_wt_2)$name,
     vertex.label.cex = 0.7,
     vertex.size = 20,
     vertex.color = "skyblue",
     vertex.label.color = "black",
     main = "Weighted network 2 (edge strengths)")

# third attempt - Transforming 'diagnosis' to binary 'is_AD'
df_3 <- df
df_3$is_AD <- ifelse(df_3$diagnosis == 4, 1, 0)
df_3$diagnosis <- NULL

# Ensuring all categorical/ordinal variables are numeric
df_3$Sex <- as.numeric(as.character(df_3$Sex))
df_3$APOE4 <- as.numeric(as.character(df_3$APOE4))
df_3$Amy_Stage <- as.numeric(as.character(df_3$Amy_Stage))
df_3$is_AD <- as.numeric(as.character(df_3$is_AD))

df_3 <- df_3 %>% mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
df_3 <- df_3 %>% mutate(across(where(is.character), as.numeric))

# Applying npn transformation
df_3_npn <- huge.npn(as.matrix(df_3)) 

# Applying graphical lasso
set.seed(1234)
glasso_fit_3 <- huge(df_3_npn, method = "glasso")
glasso_select_3 <- huge.select(glasso_fit_3, criterion = "stars")

# Plotting the structure
plot(glasso_select_3, main = "Estimated Graph using StARS (is_AD column)")

# Results
optimal_lambda_3 <- glasso_select_3$opt.lambda
cat("Optimal lambda:", optimal_lambda_3, "\n")

precision_matrix_3 <- glasso_select_3$opt.icov
adj_matrix_3 <- glasso_select_3$refit

# Sparsity
p_3 <- ncol(adj_matrix_3)
sparsity_level_3 <- sum(adj_matrix_3[upper.tri(adj_matrix_3)]) / (p_3 * (p_3 - 1) / 2)
cat("Sparsity level:", round(sparsity_level_3, 4), "\n")

# plotting
library(igraph)
node_labels_3 <- colnames(df_3)

# Binary adjacency matrix
adj_binary_3 <- (abs(precision_matrix_3) > 1e-6) * 1
diag(adj_binary_3) <- 0
rownames(adj_binary_3) <- node_labels_3
colnames(adj_binary_3) <- node_labels_3

# Weighted matrix (precision)
adj_weighted_3 <- -cov2cor(precision_matrix_3) 
diag(adj_weighted_3) <- 0
rownames(adj_weighted_3) <- node_labels_3
colnames(adj_weighted_3) <- node_labels_3
adj_weighted_3[adj_binary_3 == 0] <- 0 

# Plotting binary network
g_bin_3 <- graph.adjacency(adj_binary_3, mode = "undirected", diag = FALSE)
V(g_bin_3)$name <- node_labels_3

plot(g_bin_3,
     vertex.label = V(g_bin_3)$name,
     vertex.label.cex = 0.5,
     vertex.size = 25,
     vertex.color = "skyblue",
     vertex.label.color = "black",
     edge.color = "gray60",
     main = "Binary network 3 (edge presence)")

# Plotting weighted network
g_wt_3 <- graph.adjacency(adj_weighted_3, mode = "undirected", weighted = TRUE, diag = FALSE)
V(g_wt_3)$name <- node_labels_3

edges_3 <- get.edgelist(g_wt_3)
edge_weights_3 <- numeric(nrow(edges_3))
edge_colors_3 <- character(nrow(edges_3))

for (i in 1:nrow(edges_3)) {
  v1_3 <- edges_3[i, 1]
  v2_3 <- edges_3[i, 2]
  val_3 <- adj_weighted_3[v1_3, v2_3]
  edge_weights_3[i] <- abs(val_3) * 25
  edge_colors_3[i] <- ifelse(val_3 > 0, "green",
                             ifelse(val_3 < 0, "red", "gray70"))
}

E(g_wt_3)$weight <- edge_weights_3
E(g_wt_3)$plot_weight <- pmax(E(g_wt_3)$weight, 0.5)
E(g_wt_3)$color <- edge_colors_3

layout_fr <- layout_with_fr(g_wt_3, 
                            niter = 1200, 
                            area = vcount(g_wt_3)^2.5, 
                            repulserad = vcount(g_wt_3)^5)

plot(g_wt_3,
     layout = layout_fr,
     edge.width = E(g_wt_3)$plot_weight,
     edge.color = edge_colors_3,
     vertex.label = V(g_wt_3)$name,
     vertex.label.cex = 0.7,
     vertex.size = 20,
     vertex.color = "skyblue",
     vertex.label.color = "black",
     main = "Weighted network 3 (edge strengths and signs)")

#edge strengths trial 3
prec_mat_3 <- glasso_select_3$opt.icov

partial_corr_mat_3 <- -cov2cor(prec_mat_3)
diag(partial_corr_mat_3) <- 0  

edge_list_3 <- which(adj_binary_3 == 1, arr.ind = TRUE)

partial_corrs_3 <- data.frame(
  from = rownames(adj_binary_3)[edge_list_3[, 1]],
  to = colnames(adj_binary_3)[edge_list_3[, 2]],
  partial_correlation = partial_corr_mat_3[edge_list_3]
)

partial_corrs_3 <- partial_corrs_3[partial_corrs_3$from < partial_corrs_3$to, ]

partial_corrs_3 <- partial_corrs_3 %>%
  arrange(desc(abs(partial_correlation)))

print(partial_corrs_3)

