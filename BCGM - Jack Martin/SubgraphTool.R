######################### Create a function ####################################
### This function should have a light UI
### That allows the user to select two variables and a cutoff
### and it will display the subgraph of the paths of length < cutoff between the
### two variables.

############################# Libraries ########################################

library(BDgraph)
library(xlsx)
library(igraph)
library(ggraph)
library(tidygraph)     # gives tbl_graph, used by ggraph

############################# Load Data ########################################

setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
diagnosis_vec = c(1,2,3,4)
diag_text = paste(diagnosis_vec,collapse=" ")
filename = paste("CognitiveTests_Copula_output_diag_",diag_text,".Rdata",sep="")
load(filename)


####################### Select Parameters ######################################
From <- "Sex"
To <- "V PCC"

output = output_alg$output

graph <- BDgraph::select(output, cut=NULL, vis=FALSE)


## assume `A` is the 19 × 19 numeric matrix you showed
g <- graph_from_adjacency_matrix(graph,
                                 mode = "undirected",   # row → column = arrow direction
                                 diag = FALSE)        # ignore the 0’s on the diagonal

p_raw <- all_simple_paths(g,
                          from = From,           # start node
                          to   = To,        # target node(s)
                          mode = "out",
                          cutoff=2)              # follow arrow direction only
paths_chr <- lapply(p_raw, function(v) V(g)[v]$name)

edge_pairs <- unique(do.call(rbind, lapply(paths_chr, \(p) {
  cbind(head(p, -1), tail(p, -1))          # (v1,v2), (v2,v3)
})))

colnames(edge_pairs) <- c("from", "to")

g_sub <- graph_from_data_frame(edge_pairs, directed = TRUE)

g_tbl <- as_tbl_graph(g_sub)

ggraph(g_tbl, layout = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(3, "mm")),
                 end_cap = circle(3, "mm"),
                 width   = 0.8) +
  geom_node_circle(aes(r = 0.15), fill = "lightsteelblue") +
  geom_node_text(aes(label = name), size = 3, vjust = 1.5) +
  theme_graph()

