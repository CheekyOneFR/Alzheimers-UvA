########################### Create the Larger Model ############################

#This file does three things
#1. It loads the cleaned data
#2. It runs the algorithm
#3. It creates all the figures in the article and the figures S2 and S3 in the supplementary material

####################################################
############# libraries ##########################
####################################################

#library(readxl) #for loading excel file to R
library(circlize)
library(gplots)
library(ggplot2)
library(BDgraph)
library(RColorBrewer) #for heatmaps
library(psych)
library(gridExtra)


################################################
########1. load data########################
################################################

library(BDgraph)
setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
filename = "Cognitive_Scores_Cleaned_data.Rdata"
load(file=filename)

#######################################################
############2. run algorithms###########################
#######################################################

#define supporting functions
BD_solve = function(data,burnin_iter_vec_thin,iter_vec_thin,MCMCstart,g.prior,not.cont){
  
  #obtain p and n
  p = ncol(data)
  n = nrow(data)
  
  #parameters
  burnin = 0
  jump = 1
  save = TRUE
  cores = 11
  verbose = FALSE
  
  #run the algorithm, produce output at every iteration in the iter_vec_thin vector
  len_iter = length(iter_vec_thin)
  len_burnin = length(burnin_iter_vec_thin)
  edge_vec = rep(0,len_iter+len_burnin)
  time_vec = rep(0,len_iter+len_burnin)
  
  for (j in 1:len_burnin){
    print(burnin_iter_vec_thin[j])
    #determine amount of iterations
    olditer = 0
    if (j > 1){
      olditer = burnin_iter_vec_thin[j-1]
    }
    newiter = burnin_iter_vec_thin[j]
    iter = newiter-olditer
    
    #run bdgraph for maxiter iterations
    sample_bd  = bdgraph( data = data,algorithm = "bdmcmc",method="gcgm",not.cont=not.cont,iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=MCMCstart,g.prior=g.prior,verbose=verbose)  
    
    #save metrics
    edge_vec[j] = sum(sample_bd$last_graph)/2
    
    #save data for next run
    MCMCstart = sample_bd
    
  }
  
  #run MCMC iterations (after burnin)
  weights_old = 0
  plinks_old = 0
  K_old = 0
  plinks_iter_matrix = matrix(0,p*(p-1)/2,len_iter)
  K_iter_matrix = matrix(0,p*(p-1)/2+p,len_iter)
  for (j in 1:len_iter){
    
    print(iter_vec_thin[j])
    
    #determine amount of iterations
    olditer = 0
    if (j > 1){
      olditer = iter_vec_thin[j-1]
    }
    newiter = iter_vec_thin[j]
    iter = newiter-olditer
    
    #run bdgraph for maxiter iterations
    sample_bd  = bdgraph( data = data,algorithm = "bdmcmc",method="gcgm",not.cont=not.cont, iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=MCMCstart,g.prior=g.prior,verbose=verbose)  
    
    #calculate new p matrix
    weights_new = sum(sample_bd$graph_weights) #sample_bd$sum_weights
    #plinks_new = (weights_old*plinks_old + weights_new*sample_bd$p_links)/(weights_old+weights_new)
    plinks_new <- BDgraph::plinks(sample_bd)
    K_new = (weights_old*K_old + weights_new*sample_bd$K_hat)/(weights_old+weights_new)
    
    #save metrics
    edge_vec[len_burnin+j] = sum(sample_bd$last_graph)/2
    
    #save data for next run
    plinks_old = plinks_new
    K_old = K_new
    weights_old = weights_old+weights_new
    MCMCstart = sample_bd
    
    #save plinks in matrix
    plinks_vec = plinks_new[upper.tri(plinks_new)]
    plinks_iter_matrix[,j] = plinks_vec
    
    #save K in matrix
    K_vec = K_new[upper.tri(K_new,diag=TRUE)]
    K_iter_matrix[,j] = K_vec
    
  }
  
  print(class(sample_bd))
  plinks = plinks_new
  return(list(output=sample_bd, g.prior=g.prior,edge_vec=edge_vec,iter_vec_thin=iter_vec_thin,burnin_iter_vec_thin=burnin_iter_vec_thin,plinks=plinks_new,plinks_iter_matrix=plinks_iter_matrix,K_iter_matrix=K_iter_matrix))
} 
data_select_func = function(data,diagnosis_vec){
  
  #select subset of data according to diagnosis_vec
  index_diagnosis = which(data$Diagnosis %in% diagnosis_vec)
  data_1 = data[index_diagnosis,]
  
  #remove the diagnosis column
  data_1$Diagnosis = NULL #remove diagnosis column
  
  #output data
  return(data_1)
}
Gaussianize_func = function(data,not.cont,method){
  cont = 1 - not.cont
  index_cont = which(cont==1)
  data_cont = data[index_cont]
  if (method=="shrinkage"){data_cont_norm = bdgraph.npn(data=data_cont,npn="shrinkage")}
  if (method=="truncation"){data_cont_norm = bdgraph.npn(data=data_cont,npn="truncation")}
  if (method=="scaling"){data_cont_norm = scale(data_cont,center=TRUE,scale=TRUE)}
  data_1 = data
  data_1[index_cont] = data_cont_norm
  return(data_1)
}
run_algorithm_func = function(data,burnin_iter_vec_thin,iter_vec_thin,not.cont){
  MCMCstart = "empty"
  g.prior = 0.2
  out = BD_solve(data=data,burnin_iter_vec_thin=burnin_iter_vec_thin,iter_vec_thin=iter_vec_thin,MCMCstart=MCMCstart,g.prior=g.prior,not.cont=not.cont)
  return(out)
}

#select subset of the data, with only the diagnosis that you want included, either 1,2,3,4 or a combination
diagnosis_vec = c(1,2,3,4) 
title = paste0("Diagnosis ",paste(diagnosis_vec,collapse=" "))
print(title)
data_select = data_select_func(data=data_5,diagnosis_vec=diagnosis_vec)

#normalize continuous data
### Some of the new added cognitive test variables should be considered continuous
### Like how age is considered continuous
### MMSE LDELTOTAL	BNTSPONT	BNTTOTAL	TRAASCOR	TRABSCOR	TRAB-A	AVDEL30MIN	AVDELTOT	LIMMTOTAL
not.cont <- c(0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#not.cont=c(0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
matrix(c(colnames(data_select),not.cont),ncol=2) #check if non-continuous vector is correct (age assumed contninuos)
method = "truncation" #select either shrinkage, truncation or scaling
data_norm = Gaussianize_func(data=data_select,not.cont=not.cont,method=method)

#select the burnin and MCMC iterations, and run the algorithm

burnin_iter_vec_thin = c(10,50,100,500,1000,2500,5000,10000,20000)
iter_vec_thin = c(100,200,500,1000,2500,5000,7500,10000,15000,20000,30000,40000,50000,60000,70000,80000,90000,100000)


output_alg = run_algorithm_func(data=data_norm,burnin_iter_vec_thin=burnin_iter_vec_thin,iter_vec_thin=iter_vec_thin,not.cont=not.cont)

#save output
diag_text = paste(diagnosis_vec,collapse=" ")
filename = paste("CognitiveTests_Copula_output_diag_",diag_text,".Rdata",sep="")
save(output_alg,file=filename)