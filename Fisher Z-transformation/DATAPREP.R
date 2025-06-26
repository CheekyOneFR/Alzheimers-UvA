############################################################################################################################################
#this file does three things: 
#i) load the data from an excel file, ii) clean the data, and iii) save the cleaned data as .Rdata file
############################################################################################################################################
####################################################
############# i) load data
####################################################


#reset: rm(list = ls())


install.packages("readxl")

library(readxl) #for loading excel file to R

data_orig <- read_excel("~/Downloads/SYBREN P MEIJER/z_THESIS/2020_master_data_merged.xlsx")
data_1 = data_orig


####################################################
#############ii) clean data 
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

#check for column types and general data/matrix info 
str(data_2)
#observe there are a few (4) character columns these need to be converted to numeric:

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

#check number of individuals in each subgroup (missing values not deleted)
diagnosis_encoding = c("cognitively normal","early mild cognitive impairment","late mild cognitive impairment","dementia due to Alzheimer's disease","Subjective cognitive decline")
for (i in 1:5){
  diagnosis_vec = data_2$diagnosis == i
  total_diag = sum(diagnosis_vec)
  text= paste0(diagnosis_encoding[i]," has ",total_diag, " observations")
  print(text)
}


# Remove all rows containing NAs
sum(is.na(data_2))
data_3 = na.omit(data_2) # remove rows containing non-applicable values
rows_removed = dim(data_2)[1] - dim(data_3)[1]
rows_removed
dim(data_3)

# Standardize continuous variables

# Define continuous variables: GM and FDG brain regions + cognitive and demographic variables
brain_regions <- c("L_Hippocampus","R_Hippocampus","L_Thalamus","R_Thalamus","L_Caudate","R_Caudate",
                   "L_Putamen","R_Putamen","Cingulate_Gyrus_posterior_L","Cingulate_Gyrus_posterior_R",
                   "Precuneus_Cortex_L","Precuneus_Cortex_R")

metrics <- c("GM", "FDGpvc")
region_variables <- c()
for (metric in metrics) {
  region_variables <- c(region_variables, paste0(metric, "_", brain_regions))
}

# Continuous variables to standardize
cont_vars <- c(region_variables, "Age", "ADNI_MEM", "ADNI_EF", "EDUC", "Amy_Stage")

# Standardize them (mean = 0, sd = 1)
data_3[cont_vars] <- scale(data_3[cont_vars])

#keep APOE4 variable ordinal
APOE4_vec = data_3$APOE4
index_0 = which(APOE4_vec==0)
length(index_0)
index_1 = which(APOE4_vec==1)
length(index_1)
index_2 = which(APOE4_vec==2)
length(index_2)


#check number of individuals in each subgroup
#gives: CN 246, EMCI 297, LEMCI 205, AD 175, SCD 99
diagnosis_encoding = c("cognitively normal","early mild cognitive impairment","late mild cognitive impairment","dementia due to Alzheimer's disease","Subjective cognitive decline")
for (i in 1:5){
  diagnosis_vec = data_3$diagnosis == i
  total_diag = sum(diagnosis_vec)
  text= paste0(diagnosis_encoding[i]," has ",total_diag, " observations")
  print(text)
}

#make table 1 in the paper
#DON'T KNOW WHY BUT PLUG THE VARIABLE NAME OF INTEREST IN THE LINE OF CODE BELOW AND CALCULATE IT MANUALLY!!!
diagnosis_groups = c(1,2,3,4,5)
variable_name = "ADNI-MEM" #select one of the following variables: ADNI-EF, ADNI-MEM, Amyloid stage, Sex, APOE4, EDUC, Age
if (variable_name == "Amyloid stage"){var_vec = data_3$Amy-Stage}
if (variable_name == "Sex"){var_vec = data_3$Sex}
if (variable_name == "APOE4"){var_vec = data_3$APOE4}
if (variable_name == "EDUC"){var_vec = data_3$EDUC}
if (variable_name == "Age"){var_vec = data_3$Age}
if (variable_name == "ADNI-MEM"){var_vec = data_3$ADNI_MEM}
if (variable_name == "ADNI-EF"){var_vec = data_3$ADNI_EF}

for (diag in diagnosis_groups){
  index_diag = (data_3$diagnosis==diag)
  average = mean(var_vec[index_diag])
  sd = sd(var_vec[index_diag])
  total_female = sum(var_vec[index_diag])
  female_perc = total_female/sum(index_diag)
  text = paste0("diagnosis group ",diag," has an average of ", variable_name," of ", average, "and a standard deviation of ", sd)
  if (variable_name=="Sex"){
    text = paste0("diagnosis group ",diag," has a total of ", total_female," women. That is ",female_perc)  
  }
  print(text)
}  



####################################################
#############iii) Explaining the data
####################################################

# Check average age for males and females
mean_age_male <- mean(data_3$Age[data_3$Sex == 0], na.rm = TRUE)
mean_age_female <- mean(data_3$Age[data_3$Sex == 1], na.rm = TRUE)

# Print results
cat("Average age of males:", round(mean_age_male, 2), "\n")
cat("Average age of females:", round(mean_age_female, 2), "\n")

