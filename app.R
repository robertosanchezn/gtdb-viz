library(tidyverse)
library(glue)
library(jsonlite)
library(cowplot)
library(shiny)
library(shinymaterial)
library(ggiraph)
library(tidytree)
library(ggtree)

tax_levels <- c(
  "domain",
  "phylum",
  "class",
  "order",
  "family",
  "genus",
  "species"
)

walk(list.files("scripts", full.names = TRUE), source)
download_data()
metadata <- read_metadata("data/bac120_metadata_r214.tsv.gz")
tree <- read_and_format_tree("data/bac120_r214.tree", metadata = metadata)



# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Select a taxon"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subset_level", "Select a level:", choices = tax_levels),
      selectizeInput("subset_taxon", "Select a taxon:", choices = NULL), 
      actionButton("show", "Show tree"),
    ),
    mainPanel(
      girafeOutput("tree")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Update the selectize input choices based on the selected level
  observe({
    choices <- switch(
      input$subset_level, 
      get_all_taxa(metadata)[[input$subset_level]]
      )
    
    updateSelectizeInput(session, "subset_taxon", choices = choices, server = TRUE)
  })
  
  # One the action button is pressed, calculate the subset_tree
  
  subset_tree <- eventReactive(input$show, {
    subset_tree_to_taxon(tree, input$subset_level, input$subset_taxon)
  })
  
  
  # Display the selected text from the selectize input
  output$tree <- renderGirafe({
    
    tree_interactive <- subset_tree() |>
      ggtree() |> 
      scale_tree_to_level("genus") |> 
      annotate_taxon_nodes("genus", metadata_df = metadata) |> 
      collapse_tree_to_level("genus") 
    
    girafe(ggobj = tree_interactive)
  })

}

# Run the Shiny app
shinyApp(ui, server)
