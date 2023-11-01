library(tidyverse)
library(glue)
library(jsonlite)
library(cowplot)
library(shiny)
library(shinymaterial)
library(ggiraph)
library(tidytree)
library(ggtree)

walk(list.files("scripts", full.names = TRUE), source)

download_data()
metadata <- read_metadata("data/bac120_metadata_r214.tsv.gz")
tree <- read_and_format_tree("data/bac120_r214.tree", metadata = metadata)
all_taxa <- unique(as.vector(str_split_fixed(metadata$gtdb_taxonomy, ";", 7)))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "taxon",
        "Select a taxon",
        choices = NULL, 
        selected = "f__Bacillaceae"), 
      
      actionButton("show", "Show tree"),
      
    ), 
    mainPanel(
        "Tree", girafeOutput("tree")
      
    )
  )
)

server <- function(input, output, session){
  
  updateSelectizeInput(
    session,
    "taxon", 
    "Select a taxon",
    choices = all_taxa, 
    selected = "f__Bacillaceae", 
    server = TRUE)
  
  taxon_tree <- eventReactive(input$show, {
    subset_tree_to_taxon(tree, input$taxon)
  })
  
  output$text <- renderText(input$taxon)
  
  output$tree <- renderGirafe({
    
    tree_interactive <- taxon_tree() |> 
      ggtree() |> 
      scale_tree_to_level("genus") |> 
      annotate_taxon_nodes("genus", metadata_df = metadata) |> 
      collapse_tree_to_level("genus") 
    
    boxplot_interactive <- plot_stat_boxplot(
      tree_interactive, 
      metadata_df = metadata, 
      stat = "genome_size",
      color_stat = "gc_percentage",
      level = "genus", 
      show_datapoints = FALSE, 
    )
    girafe(
      ggobj = plot_tree_and_boxplots(
        tree_interactive,
        boxplot_interactive)
    )
  })
  
}

shinyApp(ui, server)



