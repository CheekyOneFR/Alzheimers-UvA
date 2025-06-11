############################ Load Data #########################################

setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
filename = "Cleaned_data_Lucas_Vogels.Rdata"
load(file=filename)

head(data_5)

str(data_5)

colum = 20

x = data_5[[colum]]

hist(x, xlab = colnames(data_5[,colum]) ,
     col = "pink", border = "black")


# Histogram Analysis
# 
# Age - approximately normal
# 
# Memory - approximately normal
# 
# Executive - approximately normal (slightly skewed with left tail)
# 
# Diagnosis - ordinal (more 1 than 4)
# 
# Sex - binary (approximately balanced with slightly more 0 than 1)
# 
# Education - really weird histogram distribution (kind of normally distributed but definitely skewed with a left tail and pockets of finishing education with 12 years, 14 years, 16 years, 18 years, 20 years)
# 
# APOE4 - binary (approximately balanced with slightly more 0 than 1)
# 
# Amy-stage - funky distribution with three categories (0 has the most, then 1 and 2 have approximately the same, and 3 and 4 have approximately the same)
# 
# G Thal - approximately normal
# 
# V Thal - approximately normal
# 
# G PCC - double peaked distribution maybe?
#   
#   V PCC - approximately normal
# 
# G Prec - double peaked distribution
# 
# V Prec - approximately normal
# 
# G Hipp - funky ish distribution but mostly normal
# 
# V Hipp - approximately normal
# 
# G Caud - skewed left
# 
# V Caud - approximately normal
# 
# G Put - approximately normal (with outlier?)
# 
# V Put - approximately normal
# 


########################## Load Full Data ######################################
################################################################################


library(readxl) #for loading excel file to R
setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
data_orig <- read_excel("2020_master_data_merged.xlsx")
data_1 = data_orig

head(data_1)

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

str(data_3)

################################################################################
################################################################################

########################## Investigate Double Peakedness

# G PCC
# 
# left = "Cingulate_Gyrus_posterior_L" 
# right = "Cingulate_Gyrus_posterior_R"

par(mfrow = c(2,2))

############ left

left_FDGpvc_PCC <- data_3[["FDGpvc_Cingulate_Gyrus_posterior_L"]]

hist(left_FDGpvc_PCC, xlab = "Glucose Metabolism Cingulate_Gyrus_posterior_L",
     col = "red", border = "black")

############### right

right_FDGpvc_PCC <- data_3[["FDGpvc_Cingulate_Gyrus_posterior_R"]]

hist(right_FDGpvc_PCC, xlab = "Glucose Metabolism Cingulate_Gyrus_posterior_R",
     col = "green", border = "black")

##################### Distributions are not the same, both are approximately normal
##################### around different means

combined_FDGpvc_PCC = data_5[["G PCC"]]


hist(combined_FDGpvc_PCC, xlab = "G PCC",
     col = "pink", border = "black")

# G Prec
# 
# left = "Precuneus_Cortex_L" 
# right = "Precuneus_Cortex_R"

par(mfrow = c(2,2))

############ left

left_FDGpvc_Prec <- data_3[["FDGpvc_Precuneus_Cortex_L"]]

hist(left_FDGpvc_Prec, xlab = "Glucose Metabolism Precuneus_Cortex_L",
     col = "red", border = "black")

############### right

right_FDGpvc_Prec <- data_3[["FDGpvc_Precuneus_Cortex_R"]]

hist(right_FDGpvc_Prec, xlab = "Glucose Metabolism Precuneus_Cortex_R",
     col = "green", border = "black")

##################### Distributions are not the same, both are approximately normal
##################### around different means

combined_FDGpvc_Prec = data_5[["G Prec"]]


hist(combined_FDGpvc_Prec, xlab = "G Prec",
     col = "pink", border = "black")
