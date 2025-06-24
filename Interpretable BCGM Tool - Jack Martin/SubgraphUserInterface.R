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

  #save output
  return(list(plinks=plinks,K=K,C=C))
  
}

PKC <- create_P_K_C(output_alg)

######################## Define Function

create_subgraph <- function(From, To, Degree, Method) {
  
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

  edge_df_plinks <- as.data.frame(edge_pairs, stringsAsFactors = FALSE) |>
    setNames(c("from", "to")) |>
    dplyr::mutate(weight = mapply(function(f, t) PKC$plinks[f, t], from, to))
  
  ## --- tidygraph object -----------------------------------------------------
  g_tbl <- tidygraph::as_tbl_graph(edge_df) |>
    tidygraph::activate(edges) |>
    dplyr::mutate(
      sign  = ifelse(weight >= 0, "pos", "neg"),
      abs_w = abs(weight)
    )
  
  g_tbl_plinks <- tidygraph::as_tbl_graph(edge_df_plinks) |>
    tidygraph::activate(edges) |>
    dplyr::mutate(
      sign  = ifelse(weight >= 0, "pos", "neg"),
      abs_w = abs(weight)
    )
  
  
  ## --- plot -----------------------------------------------------------------
  if (Method == "Probability") {
    plot_output <- ggraph::ggraph(g_tbl_plinks, layout = "grid") + # dh could work, gem also, grid pretty good
      # randomly too, 
      ggraph::geom_edge_link(
        aes(width = abs_w, colour = sign),
        lineend = "round"
      ) +
      ggraph::scale_edge_width(range = c(1.5, 4)) +
      ggraph::geom_node_circle(aes(r = 0.15), fill = "lightsteelblue") +
      ggraph::geom_node_text(aes(label = name), size = 5, vjust = 0) +
      ggraph::theme_graph()
  } else if (Method == "Correlation") {
    plot_output <- ggraph::ggraph(g_tbl, layout = "grid") + # dh could work, gem also, grid pretty good
      # randomly too, 
      ggraph::geom_edge_link(
        aes(width = abs_w, colour = sign),
        lineend = "round"
      ) +
      ggraph::scale_edge_width(range = c(1.5, 4)) +
      ggraph::scale_edge_colour_manual(values = c(pos = "steelblue", neg = "firebrick")) +
      ggraph::geom_node_circle(aes(r = 0.15), fill = "lightsteelblue") +
      ggraph::geom_node_text(aes(label = name), size = 5, vjust = 0) +
      ggraph::theme_graph()
  } else {}
  ## --- informative text -----------------------------------------------------
  directC <- PKC$C[From, To]
  directP <- PKC$plinks[From, To]
  totalC <- sum(edge_df$weight)
  text_output <- ""
  
  for (edge in 1:nrow(edge_df)) {
    text_output <- paste(text_output, sprintf("The partial correlation between **%s** and **%s** is %.3f, with certainty %.3f", edge_df[edge,][["from"]], edge_df[edge,][["to"]], edge_df[edge,][["weight"]], edge_df_plinks[edge,][["weight"]]))
    text_output <- paste(text_output, "\n")
  }
  
  
  ## return whatever you need
  return(list(graph  = g_tbl,
       plot   = plot_output,
       text = text_output))
}

# library(gridExtra)
# 
# plots <- list()
# 
# #Error "BNTTOTAL"
# cognitive_scores = c("MMSE", "LDELTOTAL", "BNTSPONT", "TRAB-A", "AVDEL30MIN", "AVDELTOT")
# 
# for (to in cognitive_scores) {
#   p <- try(create_subgraph("Sex", to, 2, "Probability")$plot, silent = TRUE)
#   if (!inherits(p, "try-error")) {
#     plots[[length(plots) + 1]] <- p
#   }
# }
# 
# # Show the plots in a 2x5 grid
# do.call(grid.arrange, c(plots[1:6], nrow = 3, ncol = 2))



ui <- fluidPage(
  useShinyjs(),
  useHeyshiny(language = 'en-US'),
  
  #speechInput(inputId = "hey_command", command = "hey *msg"),
  
  speechInput(inputId = "var1", command = "hey variable 1 *msg"),
  speechInput(inputId = "var1", command = "hey variable one *msg"),
  speechInput(inputId = "var2", command = "hey variable 2 *msg"),
  speechInput(inputId = "var2", command = "hey variable two *msg"),
  
  speechInput(inputId = "Degree", command = "hey degree *msg"),
  
  speechInput(inputId = "Method", command = "hey method *msg"),
  
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
        selectInput("Method", "Partial Correlations or Edge Probability:", choices = c("Correlation", "Probability")),
        actionButton("goButton", "Analyze")
        #verbatimTextOutput("shiny_response")
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

# Some variable names are title case and some are upper case

# Specifically these names are title case and the rest are upper case

# Upper case:
# Age
# Memory
# Executive
# Sex
# Educ
# Amy-stage

# I also need to think of an edge case for Amy-stage because the title case of "amy-stage"
# is "Amy-Stage"

# Server
server <- function(input, output) {
  
  # Canonical names
  new_names <- c(
    "Age","Memory","Executive", "Sex","Educ","APOE4","Amy-stage",
    "MMSE", "LDELTOTAL", "BNTSPONT", "BNTTOTAL", "TRAASCOR", "TRABSCOR", "TRAB-A",
    "AVDEL30MIN", "AVDELTOT",
    "G Thal","V Thal",
    "G PCC","V PCC",
    "G Prec","V Prec",
    "G Hipp","V Hipp",
    "G Caud","V Caud",
    "G Put","V Put"
  )
  
  # Normalize to lowercase with stripped whitespace
  normalize_input <- function(x) {
    tolower(trimws(x))
  }
  
  # Build canonical lookup table
  canonical_map <- setNames(new_names, normalize_input(new_names))
  
  # Canonicalize an input string
  canonicalize <- function(input_string) {
    normalized <- normalize_input(input_string)
    if (normalized %in% names(canonical_map)) {
      return(canonical_map[[normalized]])
    } else if (grepl("-", normalized)) {
      # Fallback for hyphenated cases
      parts <- unlist(strsplit(normalized, "-"))
      return(paste(tools::toTitleCase(parts), collapse = "-"))
    } else {
      return(tools::toTitleCase(normalized))
    }
  }
  
  
  
  observeEvent(input$goButton, {
    output$varPlot <- renderPlot({
      create_subgraph(canonicalize(input$var1), canonicalize(input$var2), input$Degree, str_to_title(input$Method))$plot
    })
    output$varText <- renderText({
      toString(create_subgraph(canonicalize(input$var1), canonicalize(input$var2), input$Degree, str_to_title(input$Method))$text)
    })
  })
  
  observeEvent(input$analyze, {
    output$varPlot <- renderPlot({
      create_subgraph(canonicalize(input$var1), canonicalize(input$var2), input$Degree, str_to_title(input$Method))$plot
    })
    output$varText <- renderText({
      create_subgraph(canonicalize(input$var1), canonicalize(input$var2), input$Degree, str_to_title(input$Method))$text
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
  
  # observeEvent(input$hey_command, {
  #   output$shiny_response <- renderText(message(input$hey_command))
  # })
}

# Run the app
shinyApp(ui = ui, server = server)
