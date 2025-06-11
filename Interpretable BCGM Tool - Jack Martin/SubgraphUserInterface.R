########################### Basic Shiny App ####################################

library(shiny)
library(shinyjs)
library(heyshiny)
library(BDgraph)
library(xlsx)
library(igraph)
library(ggraph)
library(tidygraph)     # gives tbl_graph, used by ggraph
library(stringr)

setwd("C:/Users/johne/OneDrive - UvA/Documents/Professional/Bachelor Thesis/Code and Data")
diagnosis_vec = c(1,2,3,4)
diag_text = paste(diagnosis_vec,collapse=" ")
filename = paste("CognitiveTests_Copula_output_diag_",diag_text,".Rdata",sep="")
load(filename)

var_names <- colnames(output_alg$output$data)
output <- output_alg$output

######################## Useful Data ###########################################


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
  
  #print(colnames(plinks))
  #set variable names
  old_names = colnames(plinks)
  new_names = c("Age","Memory","Executive", "Sex","Educ","APOE4","Amy-stage",
                                        "MMSE", "LDELTOTAL", "BNTSPONT", "BNTTOTAL", "TRAASCOR", "TRABSCOR", "TRAB-A",
                                        "AVDEL30MIN", "AVDELTOT",
                                        "G Thal","V Thal",
                                        "G PCC","V PCC",
                                        "G Prec","V Prec",
                                        "G Hipp","V Hipp",
                                        "G Caud","V Caud",
                                        "G Put","V Put")         
  #print(new_names)
  print(matrix(c(old_names,new_names),byrow=FALSE,ncol=2))
  colnames(plinks) = new_names
  rownames(plinks) = new_names
  colnames(K) = new_names
  rownames(K) = new_names
  colnames(C) = new_names
  rownames(C) = new_names
  
  # #reorder columns and rows
  # order_vec=c(8,10,12,14,16,18,9,11,13,15,17,19,1,4,5,6,7,2,3)
  # var_names = colnames(plinks)
  # #matrix(c(var_names,order_vec),ncol=2) #check if new labels and ordering is correct
  # print(matrix(c(var_names,order_vec),ncol=2))
  # plinks = plinks[order_vec,order_vec]
  # K = K[order_vec,order_vec]
  # C = C[order_vec,order_vec]

  #save output
  return(list(plinks=plinks,K=K,C=C))
  
}

PKC <- create_P_K_C(output_alg)

######################## Define Function

create_subgraph <- function(From, To, Degree) {
  
  ## --- BDgraph output -------------------------------------------------------
  g_full <- BDgraph::select(output_alg$output, cut = NULL, vis = FALSE) |>
    igraph::graph_from_adjacency_matrix(mode = "undirected", diag = FALSE)
  
  ## --- all paths up to 'Degree' --------------------------------------------
  p_raw <- igraph::all_simple_paths(g_full, from = From, to = To,
                                    mode = "out", cutoff = Degree)
  paths_chr <- lapply(p_raw, \(v) igraph::V(g_full)[v]$name)
  
  edge_pairs <- unique(do.call(rbind, lapply(paths_chr, \(p) {
    cbind(head(p, -1), tail(p, -1))
  })))
  ## --- edge weights from PKC$C ---------------------------------------------
  edge_df <- as.data.frame(edge_pairs, stringsAsFactors = FALSE) |>
    setNames(c("from", "to")) |>
    dplyr::mutate(weight = mapply(function(f, t) PKC$C[f, t], from, to))
  
  ## --- tidygraph object -----------------------------------------------------
  g_tbl <- tidygraph::as_tbl_graph(edge_df) |>
    tidygraph::activate(edges) |>
    dplyr::mutate(
      sign  = ifelse(weight >= 0, "pos", "neg"),
      abs_w = abs(weight)
    )
  
  
  ## --- plot -----------------------------------------------------------------
  plot_output <- ggraph::ggraph(g_tbl, layout = "sugiyama") +
    ggraph::geom_edge_link(
      aes(width = abs_w, colour = sign),
      lineend = "round"
    ) +
    ggraph::scale_edge_width(range = c(1, 4)) +
    ggraph::scale_edge_colour_manual(values = c(pos = "steelblue", neg = "firebrick")) +
    ggraph::geom_node_circle(aes(r = 0.15), fill = "lightsteelblue") +
    ggraph::geom_node_text(aes(label = name), size = 3, vjust = 1.5) +
    ggraph::theme_graph()
  
  ## --- informative text -----------------------------------------------------
  directC <- PKC$C[From, To]
  text_output <- sprintf("The partial correlation between %s and %s is %.3f",
                         From, To, directC)
  
  ## return whatever you need
  return(list(graph  = g_tbl,
       plot   = plot_output,
       text = text_output))
}


ui <- fluidPage(
  useShinyjs(),
  useHeyshiny(language = 'en-US'),
  
  speechInput(inputId = "var1", command = "hey variable 1 *msg"),
  speechInput(inputId = "var1", command = "hey variable one *msg"),
  speechInput(inputId = "var2", command = "hey variable 2 *msg"),
  speechInput(inputId = "var2", command = "hey variable two *msg"),
  
  speechInput(inputId = "Degree", command = "hey degree *msg"),
  
  speechInput(inputId = "analyze", command = "hey analyze"),
  
  titlePanel("Variable Relationship Explorer"),
  
  fluidRow(
    column(
      width = 2,
      id = "sidebar",
      div(
        id = "input-panel",
        selectInput("var1", 'Select Variable 1, or say "hey, variable one:"', choices = var_names),
        selectInput("var2", 'Select Variable 2, or say "hey, variable two:"', choices = var_names),
        selectInput("Degree", 'Select Degree: (or say "hey, degree:"', choices = c(1, 2, 3, 4, 5)),
        actionButton("goButton", "Analyze")
      ),
      actionButton("toggleSidebar", "Hide Inputs")
    ),
    
    column(
      width = 10,
      plotOutput("varPlot"),
      verbatimTextOutput("varText")
    )
  )
)


# Server
server <- function(input, output) {
  
  observeEvent(input$goButton, {
    output$varPlot <- renderPlot({
      create_subgraph(str_to_title(input$var1), str_to_title(input$var2), input$Degree)$plot
    })
    output$varText <- renderText({
      create_subgraph(str_to_title(input$var1), str_to_title(input$var2), input$Degree)$text
    })
  })
  
  observeEvent(input$analyze, {
    output$varPlot <- renderPlot({
      create_subgraph(str_to_title(input$var1), str_to_title(input$var2), input$Degree)$plot
    })
    output$varText <- renderText({
      create_subgraph(str_to_title(input$var1), str_to_title(input$var2), input$Degree)$text
    })
  })
  
  observeEvent(input$toggleSidebar, {
    is_hidden <- is.null(input$toggleSidebar) || (input$toggleSidebar %% 2 == 1)
    
    if (is_hidden) {
      hide("input-panel", anim = TRUE)
      updateActionButton(inputId = "toggleSidebar", label = "Show Inputs")
    } else {
      show("input-panel", anim = TRUE)
      updateActionButton(inputId = "toggleSidebar", label = "Hide Inputs")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
