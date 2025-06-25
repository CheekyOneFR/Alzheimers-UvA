######################## Exploratory Data Analysis #############################

library(dplyr)

######################## Load Data #############################################

library(readxl) #for loading excel file to R
setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
data_orig <- read_excel("2020_master_data_merged.xlsx")
data_1 = data_orig

########################## Look at Data ########################################

head(data_1)

str(data_1)

summary(data_1[,1:5])

# There are some numerical features that are being counted as character features
# in excel

#Create a list of these features

character_features = names(data_1)[sapply(data_1, is.character)]

character_df = data_1[character_features]

str(character_df) # 20 features, includes apoe4, summarysuvR_wholecerebnorm,
# summarysuvR_wholecerebnorm_1.11cutoff, amy_stage, ldeltotal, bntspont, bnttotal,
# traascor, trabscor, trab-A, avdel30min, avdeltot, limmtotal, cdr (cdglobal at scan),
# adni_mem, adni_ef, abeta, tau, ptau, abeta_pos

# All of these features seem like they should be numerical instead

##################### Explore Missing Values ###################################

# Count missing values in each column using sapply()
sum_na <- colSums(is.na(data_1))

has_na <- c()

has_na_greater_than_10 <- c()

for (i in seq_along(sum_na)) {
  val <- sum_na[i]
  if (val != 0) {
    has_na <- append(has_na, val)
  }
}

length(has_na) #62 na features

boxplot(has_na) #Most of the na features are skewed towards low na values

median(has_na) #median of has_na is three

Q3 <- quantile(has_na, 0.75) #Upper quartile is at 27

Q1 <- quantile(has_na, 0.25) #Lower quartile is at 1

IQR <- (Q3 - Q1)

upper_bound <- Q3 + 1.5*IQR

outliers <- has_na[has_na > upper_bound]

outliers

# Outliers Output
# 
# AV45pvc_Inferior_Temporal_Gyrus_anterior_L             AV45pvc_Frontal_Medial_Cortex_L 
#                                         120                                         141 
# AV45pvc_Temporal_Fusiform_Cortex_anterior_L  AV45pvc_Inferior_Temporal_Gyrus_anterior_R 
#                                          84                                         131 
#             AV45pvc_Frontal_Medial_Cortex_R   FDGpvc_Inferior_Temporal_Gyrus_anterior_L 
#                                         141                                         120 
#              FDGpvc_Frontal_Medial_Cortex_L  FDGpvc_Temporal_Fusiform_Cortex_anterior_L 
#                                         141                                          84 
#   FDGpvc_Inferior_Temporal_Gyrus_anterior_R              FDGpvc_Frontal_Medial_Cortex_R 
#                                         130                                         141 
# 

outlier_columns <- names(outliers)

outlier_columns
# 
# [1] "AV45pvc_Inferior_Temporal_Gyrus_anterior_L" 
# [2] "AV45pvc_Frontal_Medial_Cortex_L"            
# [3] "AV45pvc_Temporal_Fusiform_Cortex_anterior_L"
# [4] "AV45pvc_Inferior_Temporal_Gyrus_anterior_R" 
# [5] "AV45pvc_Frontal_Medial_Cortex_R"            
# [6] "FDGpvc_Inferior_Temporal_Gyrus_anterior_L"  
# [7] "FDGpvc_Frontal_Medial_Cortex_L"             
# [8] "FDGpvc_Temporal_Fusiform_Cortex_anterior_L" 
# [9] "FDGpvc_Inferior_Temporal_Gyrus_anterior_R"  
# [10] "FDGpvc_Frontal_Medial_Cortex_R"         

### Now I want to get rows from the main data frame which have missing values from
# these ten outlier features

### Dataframe --> these features --> missing values --> whole row from dataframe
# two levels of aggregation

missing_values_in_question <- apply(is.na(data_1[,outlier_columns]), 1, any)

data_with_outlier_na <- data_1[missing_values_in_question, outlier_columns]

data_with_outlier_na[1:5,]

head(data_1[,outlier_columns])

table(data_1$diagnosis[missing_values_in_question]) / length(data_1$diagnosis[missing_values_in_question])

table(data_1$diagnosis) / length(data_1$diagnosis)

# The missing values from the outliers definitely do belong to mostly subjects
# from diagnosis 2, but at the same time the ranking of proportion from each
# diagnosis stays the same, so it's not clear that this has anything to do with
# the distribution of missing values

# However it is interesting to note that diagnosis 5 doesn't have any missing
# values in the outlier columns


##################### Test Relatedness of Missing Values #######################

na_matrix <- is.na(data_1[,outlier_columns])

cor_matrix <- cor(na_matrix + 0, use = "pairwise.complete.obs")

heatmap(cor_matrix, Rowv = NA, Colv = NA, scale = "column",
        main = "heatmap(*, NA, NA) ~= image(t(x))")
