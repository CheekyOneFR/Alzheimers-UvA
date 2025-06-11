######################## Load Libraries ################################################

library(stats)
library(ggplot2)

######################## Load Data #############################################

library(readxl) #for loading excel file to R
setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
data_orig <- read_excel("2020_master_data_merged.xlsx")
data_1 = data_orig

####################################################
#############ii) clean data ########################
####################################################

#make a vector of all the variables of interested
brain_regions=c("L_Hippocampus","R_Hippocampus","L_Thalamus","R_Thalamus","L_Caudate","R_Caudate",
                "L_Putamen","R_Putamen","Cingulate_Gyrus_posterior_L","Cingulate_Gyrus_posterior_R",
                "Precuneus_Cortex_L","Precuneus_Cortex_R")
metrics = c("GM","FDGpvc") #select either GM (for volume), AV45pvc (for amyloid), or FDGpvc (for metabolism)
region_variables = c()
for (metric in metrics){
  region_variables = c(region_variables,paste0(metric,"_",brain_regions))
}
other_variables = c("Age","ADNI_MEM","ADNI_EF","diagnosis","Sex","EDUC","APOE4","Amy_Stage")
all_variables = c(region_variables,other_variables)

#select only the data of the variables of interest
data_2 = data_1[all_variables]

#change character columns to numeric columns
sapply(data_2, class) #observe that ADNI_MEM, ADNI_EF, APOE4,Amy_stage are string values
data_2$ADNI_MEM = as.numeric(data_2$ADNI_MEM)
data_2$ADNI_EF = as.numeric(data_2$ADNI_EF)
data_2$APOE4 = as.numeric(data_2$APOE4)
data_2$Amy_Stage = as.numeric(data_2$Amy_Stage)
sapply(data_2, class) #observe that all values are numeric now

#check where the non-applicable values are
na_vec = c()
for (variable_name in all_variables){
  na_vec = c(na_vec,sum(is.na(data_2[[variable_name]])))  
}
na_vec = matrix(c(all_variables,na_vec),ncol=2,byrow=FALSE)
na_vec

#remove all rows containing NAs
sum(is.na(data_2))
data_3 = na.omit(data_2) #remove rows containing non-applicable values
rows_removed = dim(data_2)[1] - dim(data_3)[1]
rows_removed
dim(data_3) 


######################## PCA ##################################################

################ standardize Data

standardized_data <- scale(data_3)

################ Run PCA

pca_result <- prcomp(standardized_data)

############### Explore PCA results

summary(pca_result)

pca_components <- pca_result$rotation

################# Visualize PCA Results

scree_data <- data.frame(
  PC = 1:length(pca_result$sdev),
  VarianceExplained = pca_result$sdev^2 / sum(pca_result$sdev^2)
)

ggplot(scree_data, aes(x = PC, y = VarianceExplained)) +
  geom_bar(stat = "identity") +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained")

