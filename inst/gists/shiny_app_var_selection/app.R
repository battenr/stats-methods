library(shiny)
library(dplyr)
library(visNetwork)
library(dagitty)

# ------------------------------------------------------
# DATA GENERATION FUNCTION
# ------------------------------------------------------
sim_data <- function(n = 200, true_effect = 1) {
  Z1 <- rnorm(n)
  Z2 <- rnorm(n)
  X  <- 0.5*Z1 + 0.5*Z2 + rnorm(n)
  M  <- 0.7*X + 0.4*Z1 + rnorm(n)
  Y  <- true_effect*X + 0.5*Z1 + 0.5*Z2 + rnorm(n)
  C  <- 0.6*X + 0.6*Y + rnorm(n)
  data.frame(Z1, Z2, X, M, C, Y)
}

# ------------------------------------------------------
# DAG DEFINITION (dagitty)
# ------------------------------------------------------
dag_definition <- dagitty("
dag {
  Z1 -> X
  Z2 -> X
  Z1 -> Y
  Z2 -> Y
  X -> M
  M -> Y
  X -> C
  Y -> C
  X -> Y
}
")

# ------------------------------------------------------
# FUNCTION: identify edges belonging to OPEN paths
# ------------------------------------------------------
get_open_edges <- function(selected_nodes) {
  adjust <- selected_nodes
  raw_paths <- paths(dag_definition, "X", "Y")$paths
  parsed_paths <- list()
  
  # parse paths
  for (i in seq_along(raw_paths)) {
    p <- raw_paths[[i]]
    if (length(p) == 1) {
      p <- unlist(strsplit(p, " "))
    }
    # remove arrows
    p <- p[!grepl("->|<-|--", p)]
    parsed_paths[[i]] <- p
  }
  
  open_edges <- list()
  for (path in parsed_paths) {
    blocked <- FALSE
    # collider logic
    for (node in path) {
      pr <- parents(dag_definition, node)
      if (length(pr) >= 2) {
        if (!(node %in% adjust)) blocked <- TRUE
      }
    }
    if (!blocked && length(path) > 1) {
      edges <- cbind(head(path, -1), tail(path, -1))
      open_edges[[length(open_edges) + 1]] <- edges
    }
  }
  
  if (length(open_edges) == 0) return(NULL)
  out <- do.call(rbind, open_edges)
  data.frame(from = out[,1], to = out[,2], stringsAsFactors = FALSE)
}

# ------------------------------------------------------
# UI
# ------------------------------------------------------
ui <- fluidPage(
  titlePanel("Interactive DAG + Bias Simulation"),
  
  tabsetPanel(
    
    # ---------------- DAG TAB ----------------
    tabPanel("DAG",
             h3("Click nodes to include/exclude them as regression controls"),
             visNetworkOutput("dagPlot", height = "500px"),
             br(),
             h4("Currently selected control variables:"),
             verbatimTextOutput("selected_nodes")
    ),
    
    # ---------------- SIMULATION TAB ----------------
    tabPanel("Simulation",
             sidebarLayout(
               sidebarPanel(
                 numericInput("n", "Sample size per simulation", 200),
                 numericInput("iters", "Number of simulations", 1000),
                 numericInput("true", "True causal effect (X → Y)", 1),
                 actionButton("run", "Run Simulation")
               ),
               mainPanel(
                 h3("Bias Results"),
                 verbatimTextOutput("biasOut"),
                 plotOutput("biasPlot")
               )
             )
    )
  )
)

# ------------------------------------------------------
# SERVER
# ------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------------- DAG NODE + EDGE SETUP ----------------
  dag_nodes <- data.frame(
    id = c("Z1", "Z2", "X", "M", "C", "Y"),
    label = c("Z1", "Z2", "X", "M", "C", "Y"),
    group = c("conf", "conf", "treat", "med", "coll", "outcome"),
    x = c(-200, 200, 0, 0, 150, 0),  # fixed horizontal positions
    y = c(-100, -100, 0, 100, 100, 200),  # fixed vertical positions
    physics = FALSE,
    stringsAsFactors = FALSE
  )
  
  dag_edges <- data.frame(
    from = c("Z1","Z2","Z1","Z2","X","M","X","Y","X"),
    to   = c("X","X","Y","Y","M","Y","C","C","Y"),
    stringsAsFactors = FALSE
  )
  
  # Track selected nodes (default: adjust for Z1, Z2)
  selected <- reactiveVal(c("Z1", "Z2"))
  
  # ---------------- CLICK HANDLER ----------------
  observeEvent(input$clicked_node, {
    node <- input$clicked_node
    if (node %in% c("X", "Y")) return()
    current <- selected()
    if (node %in% current) {
      selected(setdiff(current, node))
    } else {
      selected(c(current, node))
    }
  })
  
  # ---------------- RENDER DAG WITH HIGHLIGHTING ----------------
  output$dagPlot <- renderVisNetwork({
    # Determine open edges
    oe <- get_open_edges(selected())
    
    # default edge color = grey
    edge_colors <- rep("lightgrey", nrow(dag_edges))
    
    # highlight open paths
    if (!is.null(oe)) {
      for (i in seq_len(nrow(dag_edges))) {
        if (any(dag_edges$from[i] == oe$from & dag_edges$to[i] == oe$to)) {
          edge_colors[i] <- "#e41a1c"
        }
      }
    }
    
    # NODE COLORS
    node_df <- dag_nodes
    node_df$color.background <- "white"
    node_df$color.border <- "black"
    node_df$borderWidth <- 1
    
    node_df$color.background[node_df$id %in% selected()] <- "#fff3b0" # yellow
    node_df$color.border[node_df$id %in% selected()] <- "orange"
    node_df$borderWidth[node_df$id %in% selected()] <- 3
    
    visNetwork(
      nodes = node_df,
      edges = cbind(dag_edges, color = edge_colors)
    ) %>%
      visNodes(shape = "box", shadow = TRUE, fixed = TRUE) %>%  # fixed nodes
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE) %>% 
      visEvents(
        click = "function(nodes) {
                   if (nodes.nodes.length > 0) {
                     Shiny.setInputValue('clicked_node', nodes.nodes[0], {priority: 'event'});
                   }
                 }"
      )
  })
  
  # Show selected variables
  output$selected_nodes <- renderPrint({
    selected()
  })
  
  # ---------------- SIMULATION ----------------
  results <- eventReactive(input$run, {
    ctrl <- selected()
    formula_vars <- paste(c("X", ctrl), collapse = " + ")
    f <- as.formula(paste("Y ~", formula_vars))
    est <- numeric(input$iters)
    for (i in 1:input$iters) {
      d <- sim_data(n = input$n, true_effect = input$true)
      mod <- lm(f, data = d)
      est[i] <- coef(mod)["X"]
    }
    list(estimates = est, bias = mean(est) - input$true)
  })
  
  output$biasOut <- renderPrint({
    req(results())
    cat("Average estimated effect:", mean(results()$estimates), "\n")
    cat("True effect:", input$true, "\n")
    cat("Bias:", results()$bias, "\n")
  })
  
  output$biasPlot <- renderPlot({
    req(results())
    hist(
      results()$estimates,
      breaks = 30,
      main = "Distribution of Estimated Effects (Across Simulations)",
      xlab = "Estimated Effect",
      col = "skyblue"
    )
    abline(v = input$true, col = "red", lwd = 2)
  })
}

shinyApp(ui, server)
