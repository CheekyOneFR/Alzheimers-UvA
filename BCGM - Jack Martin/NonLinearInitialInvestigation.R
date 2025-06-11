######################### Libraries ############################################

library(ggplot2)
library(gridExtra)
library(readxl)

######################### Load Data ############################################

setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
filename = "Cleaned_data_Lucas_Vogels.Rdata"
load(file=filename)


######################## Non-Linear Plots Investigtion #########################

### If there is non-linear relationships between two different variables then we should
### find that easily within the pair-plots

### However if there are non-linear partial relationships between two different variables,
### then that is much harder to see in the graphs.

### In fact it would proabably be seen primarily in variance that cannot be captured
### within the relationship between teh two variables but 

### three variables x, y , and z
### x and y conditionally independent under z
### but x and y are both conditionaly dependent, and let's say that the relationship
### between x and z is x = a*z + b
### and y = a*z**2 + b

### So this is interesting because the way it's typically done in linear models
### with variable transformation this works perfectly fine.
### But with conditional dependence and partial correlation if z and z**2 are two
### different variables with two different distributions (though admittidely z**2
### depends entirely on z), is it possible for x and y to be conditionally independent
### under z but y not to have a linear relationship with z but a linear relationship
### with z**2

#     z
#   /   \-------\
#  x     z**2 -- y
#
# in this diagram z influences x, z**2, and y
# and z**2 influences y
# this works perfectly fine in a DAG, but does this still work in an undirected graph?



###################### Let's just practice with the data

data_6 = data_5

data_6$`Amy-stage2` <- data_6$`Amy-stage`**2

X <- data.frame(AmyStage2 = data_6[["Amy-stage2"]],
                AmyStage = data_6[["Amy-stage"]],
                Executive = data_6[["Executive"]])


p1 <- ggplot(data = X, aes(x= AmyStage2, y = Executive)) +
  geom_point()

p2 <- ggplot(data = X, aes(x = AmyStage, y = Executive)) +
  geom_point()

grid.arrange(p1, p2, nrow = 1)

### V_PCC

data_6$`V PCC`**2

data_6$`V PCC2` <- data_6$`V PCC`**2

X2 <- data.frame(VPCC = data_6[["V PCC"]],
                 VPCC2 = data_6[["V PCC2"]],
                 Executive = data_6[["Executive"]])

p21 <- ggplot(data = X2, aes(x = VPCC, y = Executive)) +
  geom_point()

p22 <- ggplot(data = X2, aes(x = VPCC2, y = Executive)) +
  geom_point()

grid.arrange(p21, p22, nrow = 1)


######################### A couple of things to think about ####################

# 1. Non-linear relationships between variables that do have partial linear relationships
# (as seen in the graphs of Vogels)

# 2. Non-linear relationships between variables that do not have partial linear relationships
# (as implied by the graphs of Vogels)

# 3. Strength of relationship (actual coefficient is likely less in a non-linear sense)

# 4. Type of non-linear relationship (set explored should be based on relevant research)

##################### Best case scenario is that you investigate through all different
# types of potential non-linear relationships between variables and then filter out only the
# most important.

