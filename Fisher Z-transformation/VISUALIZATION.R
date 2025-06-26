##########################################
# create chord diagram for All variables #
##########################################
library(circlize)

# Set to TRUE to save in pdf, set to FALSE if plot in here
SAVE_TO_PDF <- TRUE  

# Define variable name map
var_rename_map <- c(
  "GM_L_Hippocampus" = "V Hipp L", "GM_R_Hippocampus" = "V Hipp R",
  "GM_L_Thalamus" = "V Thal L", "GM_R_Thalamus" = "V Thal R",
  "GM_L_Caudate" = "V Caud L", "GM_R_Caudate" = "V Caud R",
  "GM_L_Putamen" = "V Put L", "GM_R_Putamen" = "V Put R",
  "GM_Cingulate_Gyrus_posterior_L" = "V PCC L", "GM_Cingulate_Gyrus_posterior_R" = "V PCC R",
  "GM_Precuneus_Cortex_L" = "V Prec L", "GM_Precuneus_Cortex_R" = "V Prec R",
  "FDGpvc_L_Hippocampus" = "G Hipp L", "FDGpvc_R_Hippocampus" = "G Hipp R",
  "FDGpvc_L_Thalamus" = "G Thal L", "FDGpvc_R_Thalamus" = "G Thal R",
  "FDGpvc_L_Caudate" = "G Caud L", "FDGpvc_R_Caudate" = "G Caud R",
  "FDGpvc_L_Putamen" = "G Put L", "FDGpvc_R_Putamen" = "G Put R",
  "FDGpvc_Cingulate_Gyrus_posterior_L" = "G PCC L", "FDGpvc_Cingulate_Gyrus_posterior_R" = "G PCC R",
  "FDGpvc_Precuneus_Cortex_L" = "G Prec L", "FDGpvc_Precuneus_Cortex_R" = "G Prec R",
  "Age" = "Age", "Sex" = "Sex", "EDUC" = "Educ",
  "APOE4" = "APOE4", "Amy_Stage" = "Amy-stage",
  "ADNI_MEM" = "Memory", "ADNI_EF" = "Executive"
)

# Safe renaming
safe_rename <- function(x, map) sapply(x, function(i) if (!is.na(map[i])) map[i] else i)

# Apply renaming
significant_results$Var1 <- safe_rename(significant_results$Var1, var_rename_map)
significant_results$Var2 <- safe_rename(significant_results$Var2, var_rename_map)

#stuff
significant_results$col <- ifelse(significant_results$r > 0, "blue", "red")
significant_results$weight <- abs(significant_results$r)

#Colours
group_colors <- c(
  "Age" = "#666666", "Sex" = "#666666", "Educ" = "#666666",
  "APOE4" = "#cccccc", "Amy-stage" = "#cccccc",
  "Memory" = "#222222", "Executive" = "#222222",
  setNames(rep("#eeeeee", 12), grep("^G ", unique(c(significant_results$Var1, significant_results$Var2)), value = TRUE)),
  setNames(rep("#bbbbbb", 12), grep("^V ", unique(c(significant_results$Var1, significant_results$Var2)), value = TRUE))
)

#Variable Order
var_order <- c(
  "G Hipp L", "G Hipp R", "G Thal L", "G Thal R", "G Caud L", "G Caud R",
  "G Put L", "G Put R", "G PCC L", "G PCC R", "G Prec L", "G Prec R",
  "V Hipp L", "V Hipp R", "V Thal L", "V Thal R", "V Caud L", "V Caud R",
  "V Put L", "V Put R", "V PCC L", "V PCC R", "V Prec L", "V Prec R",
  "Age", "Sex", "Educ", "APOE4", "Amy-stage", "Memory", "Executive"
)

# Optionally open PDF device
if (SAVE_TO_PDF) {
  pdf("CD_group_allvar.pdf", width = 10, height = 10)
}

# MAke the Plot
circos.clear()
circos.par(start.degree = 20, gap.degree = 4)
chordDiagram(
  x = significant_results[, c("Var1", "Var2", "weight")],
  grid.col = group_colors,
  col = significant_results$col,
  transparency = 0.2,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = 0.08),
  order = var_order
)

# Add the labels
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

#Create a Title
title("Chord Diagram - All Variables")

# Close PDF device if saving
if (SAVE_TO_PDF) {
  dev.off()
}

###########################################
# Chords per diagnosis group
###########################################

library(ppcor)
library(circlize)

# Set to TRUE to save in pdf, set to FALSE if plot in here
SAVE_TO_PDF <- TRUE  

# Diagnosis labels
diagnosis_labels <- c(
  "1" = "Cognitively Normal",
  "2" = "Early Mild Cognitive Impairment",
  "3" = "Late Mild Cognitive Impairment",
  "4" = "Alzheimer's Disease",
  "5" = "Subjective Cognitive Decline"
)

data_3[cont_vars] <- scale(data_3[cont_vars])

# Loop through each diagnosis
for (group in 1:5) {
  
  group_data <- subset(data_3, diagnosis == group)
  group_data <- group_data[, !(colnames(group_data) %in% "diagnosis")]
  numeric_data <- group_data[sapply(group_data, is.numeric)]
  
  pcor_result <- pcor(numeric_data)
  partial_corr_matrix <- pcor_result$estimate
  colnames(partial_corr_matrix) <- colnames(numeric_data)
  rownames(partial_corr_matrix) <- colnames(numeric_data)
  
  results <- data.frame(
    Var1 = character(), Var2 = character(),
    r = numeric(), CI_lower = numeric(), CI_upper = numeric(),
    significant = logical(), stringsAsFactors = FALSE
  )
  
  n_obs <- nrow(group_data)
  n_var <- ncol(partial_corr_matrix)
  se <- 1 / sqrt(n_obs - n_var - 1)
  z_crit <- qnorm(0.975)
  
  for (i in 1:(n_var - 1)) {
    for (j in (i + 1):n_var) {
      r <- partial_corr_matrix[i, j]
      if (!is.na(r)) {
        z <- 0.5 * log((1 + r) / (1 - r))
        z_low <- z - z_crit * se
        z_up  <- z + z_crit * se
        r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
        r_up  <- (exp(2 * z_up) - 1) / (exp(2 * z_up) + 1)
        is_sig <- !(0 >= r_low & 0 <= r_up)
        results <- rbind(results, data.frame(
          Var1 = rownames(partial_corr_matrix)[i],
          Var2 = colnames(partial_corr_matrix)[j],
          r = r, CI_lower = r_low, CI_upper = r_up, significant = is_sig
        ))
      }
    }
  }
  
  sig_results <- subset(results, significant == TRUE)
  sig_results$col <- ifelse(sig_results$r > 0, "blue", "red")
  sig_results$weight <- abs(sig_results$r)
  
  var_rename_map <- c(
    "GM_L_Hippocampus" = "V Hipp L", "GM_R_Hippocampus" = "V Hipp R",
    "GM_L_Thalamus" = "V Thal L", "GM_R_Thalamus" = "V Thal R",
    "GM_L_Caudate" = "V Caud L", "GM_R_Caudate" = "V Caud R",
    "GM_L_Putamen" = "V Put L", "GM_R_Putamen" = "V Put R",
    "GM_Cingulate_Gyrus_posterior_L" = "V PCC L", "GM_Cingulate_Gyrus_posterior_R" = "V PCC R",
    "GM_Precuneus_Cortex_L" = "V Prec L", "GM_Precuneus_Cortex_R" = "V Prec R",
    "FDGpvc_L_Hippocampus" = "G Hipp L", "FDGpvc_R_Hippocampus" = "G Hipp R",
    "FDGpvc_L_Thalamus" = "G Thal L", "FDGpvc_R_Thalamus" = "G Thal R",
    "FDGpvc_L_Caudate" = "G Caud L", "FDGpvc_R_Caudate" = "G Caud R",
    "FDGpvc_L_Putamen" = "G Put L", "FDGpvc_R_Putamen" = "G Put R",
    "FDGpvc_Cingulate_Gyrus_posterior_L" = "G PCC L", "FDGpvc_Cingulate_Gyrus_posterior_R" = "G PCC R",
    "FDGpvc_Precuneus_Cortex_L" = "G Prec L", "FDGpvc_Precuneus_Cortex_R" = "G Prec R",
    "Age" = "Age", "Sex" = "Sex", "EDUC" = "Educ",
    "APOE4" = "APOE4", "Amy_Stage" = "Amy-stage",
    "ADNI_MEM" = "Memory", "ADNI_EF" = "Executive"
  )
  
  safe_rename <- function(x, map) sapply(x, function(i) if (!is.na(map[i])) map[i] else i)
  sig_results$Var1 <- safe_rename(sig_results$Var1, var_rename_map)
  sig_results$Var2 <- safe_rename(sig_results$Var2, var_rename_map)
  
  group_colors <- c(
    "Age" = "#666666", "Sex" = "#666666", "Educ" = "#666666",
    "APOE4" = "#cccccc", "Amy-stage" = "#cccccc",
    "Memory" = "#222222", "Executive" = "#222222",
    setNames(rep("#eeeeee", 12), grep("^G ", unique(c(sig_results$Var1, sig_results$Var2)), value = TRUE)),
    setNames(rep("#bbbbbb", 12), grep("^V ", unique(c(sig_results$Var1, sig_results$Var2)), value = TRUE))
  )
  
  var_order <- c(
    "G Hipp L", "G Hipp R", "G Thal L", "G Thal R", "G Caud L", "G Caud R",
    "G Put L", "G Put R", "G PCC L", "G PCC R", "G Prec L", "G Prec R",
    "V Hipp L", "V Hipp R", "V Thal L", "V Thal R", "V Caud L", "V Caud R",
    "V Put L", "V Put R", "V PCC L", "V PCC R", "V Prec L", "V Prec R",
    "Age", "Sex", "Educ", "APOE4", "Amy-stage", "Memory", "Executive"
  )
  
  # Save to PDF if enabled
  if (SAVE_TO_PDF) {
    pdf(file = paste0("CD_of_group_", group, ".pdf"), width = 10, height = 10)
  }
  
  # Plot
  circos.clear()
  circos.par(start.degree = 20, gap.degree = 4)
  chordDiagram(
    sig_results[, c("Var1", "Var2", "weight")],
    grid.col = group_colors,
    col = sig_results$col,
    transparency = 0.2,
    annotationTrack = "grid",
    preAllocateTracks = list(track.height = 0.08),
    order = var_order
  )
  
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
  
  title(paste("Chord Diagram -", diagnosis_labels[as.character(group)]))
  
  # Close device if saving
  if (SAVE_TO_PDF) {
    dev.off()
    cat("Saved chord diagram for group", group, "to PDF.\n")
  }
}

######################################################
# Create heatmap for the All variables chord diagram #
######################################################


# Set to TRUE to save in pdf, set to FALSE if plot in here
save_to_pdf <- TRUE  

library(pheatmap)

# Define abbreviation map
var_rename_map <- c(
  "GM_L_Hippocampus" = "V Hipp L", "GM_R_Hippocampus" = "V Hipp R",
  "GM_L_Thalamus" = "V Thal L", "GM_R_Thalamus" = "V Thal R",
  "GM_L_Caudate" = "V Caud L", "GM_R_Caudate" = "V Caud R",
  "GM_L_Putamen" = "V Put L", "GM_R_Putamen" = "V Put R",
  "GM_Cingulate_Gyrus_posterior_L" = "V PCC L", "GM_Cingulate_Gyrus_posterior_R" = "V PCC R",
  "GM_Precuneus_Cortex_L" = "V Prec L", "GM_Precuneus_Cortex_R" = "V Prec R",
  "FDGpvc_L_Hippocampus" = "G Hipp L", "FDGpvc_R_Hippocampus" = "G Hipp R",
  "FDGpvc_L_Thalamus" = "G Thal L", "FDGpvc_R_Thalamus" = "G Thal R",
  "FDGpvc_L_Caudate" = "G Caud L", "FDGpvc_R_Caudate" = "G Caud R",
  "FDGpvc_L_Putamen" = "G Put L", "FDGpvc_R_Putamen" = "G Put R",
  "FDGpvc_Cingulate_Gyrus_posterior_L" = "G PCC L", "FDGpvc_Cingulate_Gyrus_posterior_R" = "G PCC R",
  "FDGpvc_Precuneus_Cortex_L" = "G Prec L", "FDGpvc_Precuneus_Cortex_R" = "G Prec R",
  "Age" = "Age", "Sex" = "Sex", "EDUC" = "Educ",
  "APOE4" = "APOE4", "Amy_Stage" = "Amy-stage",
  "ADNI_MEM" = "Memory", "ADNI_EF" = "Executive"
)

safe_rename <- function(x, map) sapply(x, function(i) if (!is.na(map[i])) map[i] else i)

results$Var1 <- safe_rename(results$Var1, var_rename_map)
results$Var2 <- safe_rename(results$Var2, var_rename_map)

var_names <- sort(unique(c(results$Var1, results$Var2)))

#####################################################
# Heatmap 1: NA for non-significant correlations
#####################################################

sig_matrix_na <- matrix(NA, nrow = length(var_names), ncol = length(var_names),
                        dimnames = list(var_names, var_names))

for (i in 1:nrow(results)) {
  row <- results[i, ]
  if (row$significant) {
    sig_matrix_na[row$Var1, row$Var2] <- row$r
    sig_matrix_na[row$Var2, row$Var1] <- row$r
  }
}
diag(sig_matrix_na) <- 1

if (save_to_pdf) {
  pdf("HM_group_AllVarNA.pdf", width = 15, height = 10)
}
pheatmap(sig_matrix_na,
         display_numbers = TRUE,
         na_col = "white",
         color = colorRampPalette(c("red", "white", "blue"))(100),
         breaks = seq(-1, 1, length.out = 101),
         main = "Significant Partial Correlations (95% CI)",
         fontsize = 9,
         cluster_rows = FALSE,
         cluster_cols = FALSE)
if (save_to_pdf) {
  dev.off()
}

#####################################################
# Heatmap 2: 0 for non-significant correlations
#####################################################

sig_matrix_zero <- matrix(0, nrow = length(var_names), ncol = length(var_names),
                          dimnames = list(var_names, var_names))

for (i in 1:nrow(results)) {
  row <- results[i, ]
  if (row$significant) {
    sig_matrix_zero[row$Var1, row$Var2] <- row$r
    sig_matrix_zero[row$Var2, row$Var1] <- row$r
  }
}
diag(sig_matrix_zero) <- 1

if (save_to_pdf) {
  pdf("HM_group_allVAr.pdf", width = 15, height = 10)
}
pheatmap(sig_matrix_zero,
         display_numbers = TRUE,
         color = colorRampPalette(c("red", "white", "blue"))(100),
         breaks = seq(-1, 1, length.out = 101),
         main = "Partial Correlations (Non-significant set to 0)",
         fontsize = 9,
         cluster_rows = FALSE,
         cluster_cols = FALSE)
if (save_to_pdf) {
  dev.off()
}


################################################
# Heatmap per diagnosis group 
################################################

library(ppcor)
library(pheatmap)

# Set to FALSE to disable PDF saving
save_to_pdf <- TRUE  

# Abbreviations
var_rename_map <- c(
  "GM_L_Hippocampus" = "V Hipp L", "GM_R_Hippocampus" = "V Hipp R",
  "GM_L_Thalamus" = "V Thal L", "GM_R_Thalamus" = "V Thal R",
  "GM_L_Caudate" = "V Caud L", "GM_R_Caudate" = "V Caud R",
  "GM_L_Putamen" = "V Put L", "GM_R_Putamen" = "V Put R",
  "GM_Cingulate_Gyrus_posterior_L" = "V PCC L", "GM_Cingulate_Gyrus_posterior_R" = "V PCC R",
  "GM_Precuneus_Cortex_L" = "V Prec L", "GM_Precuneus_Cortex_R" = "V Prec R",
  "FDGpvc_L_Hippocampus" = "G Hipp L", "FDGpvc_R_Hippocampus" = "G Hipp R",
  "FDGpvc_L_Thalamus" = "G Thal L", "FDGpvc_R_Thalamus" = "G Thal R",
  "FDGpvc_L_Caudate" = "G Caud L", "FDGpvc_R_Caudate" = "G Caud R",
  "FDGpvc_L_Putamen" = "G Put L", "FDGpvc_R_Putamen" = "G Put R",
  "FDGpvc_Cingulate_Gyrus_posterior_L" = "G PCC L", "FDGpvc_Cingulate_Gyrus_posterior_R" = "G PCC R",
  "FDGpvc_Precuneus_Cortex_L" = "G Prec L", "FDGpvc_Precuneus_Cortex_R" = "G Prec R",
  "Age" = "Age", "Sex" = "Sex", "EDUC" = "Educ",
  "APOE4" = "APOE4", "Amy_Stage" = "Amy-stage",
  "ADNI_MEM" = "Memory", "ADNI_EF" = "Executive"
)

safe_rename <- function(x, map) {
  sapply(x, function(i) if (!is.na(map[i])) map[i] else i)
}

diagnosis_labels <- c(
  "1" = "Cognitively Normal",
  "2" = "Early Mild Cognitive Impairment",
  "3" = "Late Mild Cognitive Impairment",
  "4" = "Alzheimer's Disease",
  "5" = "Subjective Cognitive Decline"
)

data_3[cont_vars] <- scale(data_3[cont_vars])


for (group in 1:5) {
  
  group_data <- subset(data_3, diagnosis == group)
  group_data <- group_data[, !(colnames(group_data) %in% "diagnosis")]
  numeric_data <- group_data[sapply(group_data, is.numeric)]
  
  pcor_result <- pcor(numeric_data)
  partial_corr_matrix <- pcor_result$estimate
  colnames(partial_corr_matrix) <- colnames(numeric_data)
  rownames(partial_corr_matrix) <- colnames(numeric_data)
  
  results <- data.frame(Var1 = character(), Var2 = character(),
                        r = numeric(), CI_lower = numeric(), CI_upper = numeric(),
                        significant = logical(), stringsAsFactors = FALSE)
  
  n_obs <- nrow(group_data)
  n_var <- ncol(partial_corr_matrix)
  se <- 1 / sqrt(n_obs - n_var - 1)
  z_crit <- qnorm(0.975)
  
  for (i in 1:(n_var - 1)) {
    for (j in (i + 1):n_var) {
      r <- partial_corr_matrix[i, j]
      if (!is.na(r)) {
        z <- 0.5 * log((1 + r) / (1 - r))
        z_low <- z - z_crit * se
        z_up  <- z + z_crit * se
        r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
        r_up  <- (exp(2 * z_up) - 1) / (exp(2 * z_up) + 1)
        is_sig <- !(0 >= r_low & 0 <= r_up)
        results <- rbind(results, data.frame(
          Var1 = rownames(partial_corr_matrix)[i],
          Var2 = colnames(partial_corr_matrix)[j],
          r = r, CI_lower = r_low, CI_upper = r_up, significant = is_sig
        ))
      }
    }
  }
  
  results$Var1 <- safe_rename(results$Var1, var_rename_map)
  results$Var2 <- safe_rename(results$Var2, var_rename_map)
  var_names <- sort(unique(c(results$Var1, results$Var2)))
  
  sig_matrix <- matrix(0, nrow = length(var_names), ncol = length(var_names),
                       dimnames = list(var_names, var_names))
  
  for (i in 1:nrow(results)) {
    row <- results[i, ]
    if (row$significant) {
      sig_matrix[row$Var1, row$Var2] <- row$r
      sig_matrix[row$Var2, row$Var1] <- row$r
    }
  }
  diag(sig_matrix) <- 1
  
  if (save_to_pdf) {
    pdf(paste0("HM_group", group, ".pdf"), width = 15, height = 10)
  }
  
  pheatmap(sig_matrix,
           display_numbers = TRUE,
           na_col = "white",
           color = colorRampPalette(c("red", "white", "blue"))(100),
           breaks = seq(-1, 1, length.out = 101),
           main = paste("Heatmap -", diagnosis_labels[as.character(group)]),
           fontsize = 9,
           cluster_rows = FALSE,
           cluster_cols = FALSE)
  
  if (save_to_pdf) {
    dev.off()
  }
}



############################################
# Heatmap PEARSON CORRELATION HEATMAP
###########################################
# Load required packages
library(pheatmap)

# Step 1: Rename map
var_rename_map <- c(
  "GM_L_Hippocampus" = "V Hipp L", "GM_R_Hippocampus" = "V Hipp R",
  "GM_L_Thalamus" = "V Thal L", "GM_R_Thalamus" = "V Thal R",
  "GM_L_Caudate" = "V Caud L", "GM_R_Caudate" = "V Caud R",
  "GM_L_Putamen" = "V Put L", "GM_R_Putamen" = "V Put R",
  "GM_Cingulate_Gyrus_posterior_L" = "V PCC L", "GM_Cingulate_Gyrus_posterior_R" = "V PCC R",
  "GM_Precuneus_Cortex_L" = "V Prec L", "GM_Precuneus_Cortex_R" = "V Prec R",
  "FDGpvc_L_Hippocampus" = "G Hipp L", "FDGpvc_R_Hippocampus" = "G Hipp R",
  "FDGpvc_L_Thalamus" = "G Thal L", "FDGpvc_R_Thalamus" = "G Thal R",
  "FDGpvc_L_Caudate" = "G Caud L", "FDGpvc_R_Caudate" = "G Caud R",
  "FDGpvc_L_Putamen" = "G Put L", "FDGpvc_R_Putamen" = "G Put R",
  "FDGpvc_Cingulate_Gyrus_posterior_L" = "G PCC L", "FDGpvc_Cingulate_Gyrus_posterior_R" = "G PCC R",
  "FDGpvc_Precuneus_Cortex_L" = "G Prec L", "FDGpvc_Precuneus_Cortex_R" = "G Prec R",
  "Age" = "Age", "Sex" = "Sex", "EDUC" = "Educ",
  "APOE4" = "APOE4", "Amy_Stage" = "Amy-stage",
  "ADNI_MEM" = "Memory", "ADNI_EF" = "Executive"
)

safe_rename <- function(x, map) sapply(x, function(i) if (!is.na(map[i])) map[i] else i)

# Select numeric variables excluding diagnosis
numeric_data <- data_3[sapply(data_3, is.numeric)]
numeric_data <- numeric_data[, !(colnames(numeric_data) %in% "diagnosis")]


pearson_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
n <- nrow(numeric_data)

get_p <- function(r, n) {
  t_val <- r * sqrt((n - 2) / (1 - r^2))
  2 * pt(-abs(t_val), df = n - 2)
}

pval_matrix <- matrix(NA, ncol = ncol(pearson_matrix), nrow = nrow(pearson_matrix))
rownames(pval_matrix) <- colnames(pval_matrix) <- colnames(pearson_matrix)

for (i in 1:ncol(pearson_matrix)) {
  for (j in 1:nrow(pearson_matrix)) {
    r <- pearson_matrix[i, j]
    if (!is.na(r) && i != j) {
      pval_matrix[i, j] <- get_p(r, n)
    } else {
      pval_matrix[i, j] <- 0
    }
  }
}

masked_pearson <- pearson_matrix
masked_pearson[pval_matrix > 0.05] <- 0
diag(masked_pearson) <- 1

# Apply variable name mapping to Pearson matrix
short_names <- safe_rename(colnames(masked_pearson), var_rename_map)
colnames(masked_pearson) <- short_names
rownames(masked_pearson) <- short_names

pheatmap(masked_pearson,
         display_numbers = TRUE,
         color = colorRampPalette(c("red", "white", "blue"))(100),
         breaks = seq(-1, 1, length.out = 101),
         main = "Pearson Correlations (p < 0.05)",
         fontsize = 9,
         cluster_rows = FALSE,
         cluster_cols = FALSE)




################################################
# further calculations of the results
################################################


# AVERAGE PARTIAL CORRELATION 

# Remove diagonal (set to NA) to avoid including self-correlations
diag(partial_matrix) <- NA

# Compute mean of non-NA values
avg_partial_corr <- mean(partial_matrix, na.rm = TRUE)
cat("Average Partial Correlation (off-diagonal):", avg_partial_corr, "\n")


# AVERAGE PEARSON CORRELATION
# Use the same `masked_pearson` matrix as built earlier (with p > 0.05 set to 0)

# Remove diagonal (set to NA)
diag(masked_pearson) <- NA

# Compute mean of non-zero (significant) correlations
avg_pearson_corr <- mean(masked_pearson[masked_pearson != 0], na.rm = TRUE)
cat("Average Pearson Correlation (significant, off-diagonal):", avg_pearson_corr, "\n")

################################################

# STRONG PARTIAL CORRELATIONS

# Exclude diagonal
partial_vals <- partial_matrix[lower.tri(partial_matrix)]
# Count how many are strong
n_strong_partial <- sum(abs(partial_vals) > 0.25, na.rm = TRUE)
# Total number of valid correlations
n_total_partial <- sum(!is.na(partial_vals))
# Proportion
prop_strong_partial <- n_strong_partial / n_total_partial
cat("Strong Partial Correlations (|r| > 0.25):", n_strong_partial, "out of", n_total_partial,
    sprintf("(%.2f%%)", 100 * prop_strong_partial), "\n")


# STRONG PEARSON CORRELATIONS 

# Exclude diagonal
pearson_vals <- masked_pearson[lower.tri(masked_pearson)]
# Remove 0s (non-significant correlations)
pearson_vals <- pearson_vals[pearson_vals != 0]
# Count strong ones
n_strong_pearson <- sum(abs(pearson_vals) > 0.25)
n_total_pearson <- length(pearson_vals)
prop_strong_pearson <- n_strong_pearson / n_total_pearson
cat("Strong Pearson Correlations (|r| > 0.25):", n_strong_pearson, "out of", n_total_pearson,
    sprintf("(%.2f%%)", 100 * prop_strong_pearson), "\n")



#####################
# calc important pc's
######################
# Thresholds
lower_bound <- 0.25
upper_bound <- 0.6

# Loop through diagnosis groups
for (group in 1:5) {
  cat("\n====", diagnosis_labels[as.character(group)], "====\n")
  
  group_data <- subset(data_3, diagnosis == group)
  group_data <- group_data[, !(colnames(group_data) %in% "diagnosis")]
  numeric_data <- group_data[sapply(group_data, is.numeric)]
  
  pcor_result <- pcor(numeric_data)
  partial_corr_matrix <- pcor_result$estimate
  colnames(partial_corr_matrix) <- colnames(numeric_data)
  rownames(partial_corr_matrix) <- colnames(numeric_data)
  
  # Significance testing
  results <- data.frame(Var1 = character(), Var2 = character(),
                        r = numeric(), CI_lower = numeric(), CI_upper = numeric(),
                        significant = logical(), stringsAsFactors = FALSE)
  
  n_obs <- nrow(group_data)
  n_var <- ncol(partial_corr_matrix)
  se <- 1 / sqrt(n_obs - n_var - 1)
  z_crit <- qnorm(0.975)
  
  for (i in 1:(n_var - 1)) {
    for (j in (i + 1):n_var) {
      r <- partial_corr_matrix[i, j]
      if (!is.na(r)) {
        z <- 0.5 * log((1 + r) / (1 - r))
        z_low <- z - z_crit * se
        z_up  <- z + z_crit * se
        r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
        r_up  <- (exp(2 * z_up) - 1) / (exp(2 * z_up) + 1)
        is_sig <- !(0 >= r_low & 0 <= r_up)
        results <- rbind(results, data.frame(
          Var1 = rownames(partial_corr_matrix)[i],
          Var2 = colnames(partial_corr_matrix)[j],
          r = r, CI_lower = r_low, CI_upper = r_up, significant = is_sig
        ))
      }
    }
  }
  
  # Rename with abbreviations
  results$Var1 <- safe_rename(results$Var1, var_rename_map)
  results$Var2 <- safe_rename(results$Var2, var_rename_map)
  
  # Filter by range and significance
  filtered_results <- subset(results, significant & abs(r) >= lower_bound & abs(r) <= upper_bound)
  
  # Show table
  print(filtered_results[order(-abs(filtered_results$r)), ])
  
  # Compute sum
  sum_filtered <- sum(abs(filtered_results$r))
  cat("Sum of partial correlations between 0.25 and 0.6:", round(sum_filtered, 3), "\n")
}



