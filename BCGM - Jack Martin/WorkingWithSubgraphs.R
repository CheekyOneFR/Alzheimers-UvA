############################# View Graphs ######################################

############################# Libraries ########################################

library(BDgraph)
library(xlsx)

############################# Load Data ########################################

setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
diagnosis_vec = c(1,2,3,4)
diag_text = paste(diagnosis_vec,collapse=" ")
filename = paste("WithOutput_Copula_output_diag_",diag_text,".Rdata",sep="")
load(filename)

output = output_alg$output

class(output)

select(output, cut = NULL, vis = TRUE)

K <- output$K_hat

K



create_P_K_C = function(output_alg){
  
  #load p and K_iter_matrix
  plinks = output_alg$plinks
  K_iter_matrix = output_alg$K_iter_matrix
  
  #create the symmetric plinks matrix
  p = dim(plinks)[1]
  count = 0
  for (i in 2:p){ #fill lower matrix
    for (j in 1:(i-1)){
      count = count + 1
      plinks[i,j] = plinks[j,i]
    } 
  }
  
  #create the symmetric precision matrix
  final_iter = dim(K_iter_matrix)[2]
  K_vec = K_iter_matrix[,final_iter]
  K = matrix(0,p,p)
  K[upper.tri(K,diag=TRUE)] = K_vec
  count = 0
  for (i in 2:p){ #fill lower matrix
    for (j in 1:(i-1)){
      count = count + 1
      K[i,j] = K[j,i]
    } 
  }
  
  #create the partial correlations matrix
  K_diag_vec = 1/sqrt(diag(K))
  K_diag = diag(K_diag_vec)
  C = -K_diag%*%K%*%K_diag
  
  #set variable names
  old_names = colnames(plinks)
  new_names = new_names = c("Age","Memory","Executive","Sex","Educ","APOE4","Amy-stage",
                            "G Thal","V Thal",
                            "G PCC","V PCC",
                            "G Prec","V Prec",
                            "G Hipp","V Hipp",
                            "G Caud","V Caud",
                            "G Put","V Put")      
  matrix(c(old_names,new_names),byrow=FALSE,ncol=2)
  colnames(plinks) = new_names
  rownames(plinks) = new_names
  colnames(K) = new_names
  rownames(K) = new_names
  colnames(C) = new_names
  rownames(C) = new_names
  
  #reorder columns and rows
  order_vec=c(8,10,12,14,16,18,9,11,13,15,17,19,1,4,5,6,7,2,3)
  var_names = colnames(plinks)
  matrix(c(var_names,order_vec),ncol=2) #check if new labels and ordering is correct
  plinks = plinks[order_vec,order_vec]
  K = K[order_vec,order_vec]
  C = C[order_vec,order_vec]
  
  #save output
  return(list(plinks=plinks,K=K,C=C))
  
}

PKC <- create_P_K_C(output_alg)

PKC$C


output$method



########## There seems to be at least 3 paths between Executive function and Memory
#### Plus potentially more hidden ones

########## How can I find all unique paths between Executive function and memory?


graph <- select(output, cut=NULL, vis=TRUE)

################# Use igraph

library(igraph)

## assume `A` is the 19 × 19 numeric matrix you showed
g <- graph_from_adjacency_matrix(graph,
                                 mode = "undirected",   # row → column = arrow direction
                                 diag = FALSE)        # ignore the 0’s on the diagonal

summary(g)



p_raw <- all_simple_paths(g,
                          from = "Age",           # start node
                          to   = "Executive",        # target node(s)
                          mode = "out",
                          cutoff=2)              # follow arrow direction only

str(p_raw[1])

######################## Get a list with just the Paths ########################

paths_chr <- lapply(p_raw, function(v) V(g)[v]$name)

max_len <- max(lengths(paths_chr))                 # longest path
pad      <- function(x, n) c(x, rep(NA, n - length(x)))

df <- do.call(rbind, lapply(paths_chr, pad, n = max_len))
df <- as.data.frame(df, stringsAsFactors = FALSE)

colnames(df) <- paste0("Step_", seq_len(ncol(df))) # Step_1, Step_2, …
rownames(df) <- NULL                               # cleaner sheet

df

################### Create and plot subgraph ###################################

edge_pairs <- unique(do.call(rbind, lapply(paths_chr, \(p) {
  cbind(head(p, -1), tail(p, -1))          # (v1,v2), (v2,v3)
})))

colnames(edge_pairs) <- c("from", "to")

g_sub <- graph_from_data_frame(edge_pairs, directed = TRUE)

library(ggraph)
library(tidygraph)     # gives tbl_graph, used by ggraph

g_tbl <- as_tbl_graph(g_sub)

ggraph(g_tbl, layout = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(3, "mm")),
                 end_cap = circle(3, "mm"),
                 width   = 0.8) +
  geom_node_circle(aes(r = 0.15), fill = "lightsteelblue") +
  geom_node_text(aes(label = name), size = 3, vjust = 1.5) +
  theme_graph()


### Save Graph
write.xlsx(graph, "graph.xlsx", sheetName = "VogelsGraph", col.names = TRUE, row.names = TRUE)

### Save PKC
write.xlsx(PKC, "PLinks_PrecisionMatrix_CorrelationMatrix.xlsx", sheetName = "PKC", col.names = TRUE, row.names = TRUE)

### Save p_raw

write.xlsx(df, "Simple_Paths_Age_Executive.xlsx", sheetName="p_raw", col.names = TRUE, row.names = FALSE)
