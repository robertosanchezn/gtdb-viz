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

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Select a taxon"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("subset_level", "Select a level:", choices = tax_levels),
      
      selectizeInput("subset_taxon", "Select a taxon:", choices = NULL), 
      
      selectInput("group_level", "Select a level to group", choices = NULL),
      
      selectInput(
        "variable_x",
        "Select a variable to plot",
        choices = c("gc_percentage", "genome_size", "coding_density")),
      
      # Checkbox to show bootstraps
      checkboxInput("show_bootstraps", "Show bootstraps", value = FALSE),
      
      checkboxInput("show_datapoints", "Show individual datapoints", value = FALSE),
      
      actionButton("show", "Show tree"),
    ),
    mainPanel(
      girafeOutput("tree")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  walk(list.files("scripts", full.names = TRUE), source)
  download_data()
  metadata <- read_metadata("data/bac120_metadata_r214.tsv.gz")
  tree <- read_and_format_tree("data/bac120_r214.tree", metadata = metadata)
  
  
  # Update the selectize input choices based on the selected level
  observe({
    taxa_choices <- switch(
      input$subset_level, 
      get_all_taxa(metadata)[[input$subset_level]]
      )
    
    # Update the selectize with the corresponding choices
    # TODO:, if you use named vectors, a label different from the value 
    # is displayed. Use this to display something like 
    # Taxon_a ({n_genome} genomes)
    
    updateSelectizeInput(
      session, 
      "subset_taxon",
      choices=taxa_choices, 
      server=TRUE
      )
    
    updateSelectInput(
      session, 
      "group_level",
      # Only levels lower than the subset level are allowed
      choices=tax_levels[(which(tax_levels == input$subset_level) + 1):7]
      )
    
  })
  
  # Once the action button is pressed, calculate the subset_tree
  
  subset_tree <- eventReactive(input$show, {
    subset_tree_to_taxon(tree, input$subset_level, input$subset_taxon)
  })
  
  
  # Display the girafe plot
  output$tree <- renderGirafe({
    
    tree_interactive <- subset_tree() |>
      ggtree() |> 
      scale_tree_to_level(input$group_level) |> 
      annotate_taxon_nodes(input$group_level, metadata_df = metadata) |> 
      collapse_tree_to_level(input$group_level) |> 
      add_tips()
    
    if (input$show_bootstraps) {
      tree_interactive <- add_bootstraps(tree_interactive)
    }
    
      boxplot_interactive <- plot_stat_boxplot(
        tree_interactive, 
        stat = input$variable_x,
        metadata = metadata,
        level = input$group_level,
        show_datapoints = input$show_datapoints 
      )
      
      girafe(
        ggobj = plot_tree_and_boxplots(
          tree_interactive,
          boxplot_interactive)
      )
  })

}

# Run the Shiny app
shinyApp(ui, server)
