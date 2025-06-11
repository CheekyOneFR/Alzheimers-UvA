#load library
library("readxl")

# Load your real ADNI data
adni <- read_excel("2020_master_data_merged.xlsx")


# Check for missing values
colSums(is.na(adni))

# Option: impute or remove missing data
data_adni <- na.omit(adni)

# Convert categorical to numeric (if needed)
data_adni$Sex <- ifelse(data_adni$Sex == "Female", 1, 0)
data_adni$Education <- as.integer(data_adni$Education)  # ensure it's ordinal


# Fit the Gaussian Copula Graphical Model
library(BDgraph)
result_adni <- bdgraph(data = data_adni, method = "gcgm", iter = 10000, save = TRUE)

# Plot the selected graph
plot(result_adni, main = "Estimated Graph Structure from ADNI")

# Posterior summary
summary(result_adni)
