############################ Libraries #########################################
# GGPlot for visualization
library(ggplot2)
library(gridExtra)


############################ Load Data #########################################

setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
filename = "Cleaned_data_Lucas_Vogels.Rdata"
load(file=filename)

head(data_5)

str(data_5)

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




############# Investigate finding of bimodal glucose metabolism combination ####

left_FDGpvc_PCC <- data_3[["FDGpvc_Cingulate_Gyrus_posterior_L"]]

right_FDGpvc_PCC <- data_3[["FDGpvc_Cingulate_Gyrus_posterior_R"]]

combined_FDGpvc_PCC = data_5[["G PCC"]]

############# In the histogram analysis we found that when you average the left
###### and right values from the glucose metabolism of the PCC there are two modes

### What could this be caused by?

### When you average out the left and right hemispheres there is a proportion
### of the sample that has a higher than the mode average glucose metabolism

### First let's see if we can rule out demographic explanation

colnames(data_5)

X <- data.frame(G_PCC = data_5[["G PCC"]],
                Age = data_5[["Age"]],
                Sex = data_5[["Sex"]],
                Education = data_5[["Educ"]])

### Demographic variables include Age, Sex, and Education

Age <- data_5[["Age"]]

Sex <- data_5[["Sex"]]

Education <- data_5[["Educ"]]

###### Scatter Plots

ggplot(data = X, aes(x = Education, y = G_PCC)) + ### Select Education, Age, or Sex
  geom_point()


### Binning

XSex0 <- data.frame(G_PCC = data_5[data_5$Sex == 0, "G PCC"])

p1 <- ggplot(data = XSex0, aes(x = G.PCC)) +
  geom_histogram()

XSex1 <- data.frame(G_PCC = data_5[data_5$Sex == 1, "G PCC"])

p2 <- ggplot(data = XSex1, aes(x = G.PCC)) +
  geom_histogram()

grid.arrange(p1, p2, nrow = 1)

mean(XSex0$G.PCC)

mean(XSex1$G.PCC) ########## Sex 0 has a slightly higher average G.PCC


XEducation_1std <- data.frame(G_PCC = data_5[data_5$Educ > (mean(data_5$Educ) + var(data_5$Educ)**(1/2)), "G PCC"])

XEducation_less1std <- data.frame(G_PCC = data_5[data_5$Educ > (mean(data_5$Educ) - var(data_5$Educ)**(1/2)), "G PCC"])

mean(XEducation_1std$G.PCC)

mean(XEducation_less1std$G.PCC) ######## Around the same mean

XAge_1std <- data.frame(G_PCC = data_5[data_5$Age > (mean(data_5$Age) + var(data_5$Age)**(1/2)), "G PCC"])

XAge_less1std <- data.frame(G_PCC = data_5[data_5$Age > (mean(data_5$Age) - var(data_5$Age)**(1/2)), "G PCC"])

mean(XAge_1std$G.PCC)

mean(XAge_less1std$G.PCC) ######## Also around the same mean


##################### Conclusion ###############################################
# 
# While sex 0 has a slightly higher mean glucose metabolism in their PCC, it is
# not quite enough to cause the bimodalisation of the averaged distribution. Furthermore
# none of the other demographic features have the power to make a second mode around
# 1.7 either


############ Next thing to investigate: is there a relationship between this bimodalization
############ and the other factors (memory, executive function, diagnosis)

