

#' Scale all the clades at a given taxonomic level so they all take the 
#' same space on the y axis
#'
#' @param treeplot a ggtree object
#' @param level 
#'
#' @return a scaled ggtree object
#' 
scale_tree_to_level <- function(treeplot, level) {
  
  letter <- str_sub(level, 1, 1)
  pattern <- paste0(letter, "__")
  
  nodes <- treeplot$data |>
    filter(str_detect(taxon, pattern)) |>
    pull(node) |> 
    as.numeric()
  
  
  treedata <- as.treedata(treeplot$data)
  
  scaled_treeplot <- treeplot
  
  for (node in nodes) {
    
    scale <- 1 / length(offspring(treedata, node, type = "tips"))
      
    scaled_treeplot <- scaleClade(scaled_treeplot, node, scale)
    
  }
  
  scaled_treeplot
  
}

#' Collapse all the clades at a given taxonomic level, and represents them 
#' with a triangle showing the farthest and nearest tips
#'
#' @param treeplot a ggtree object
#' @param level 
#'
#' @return a collapsed ggtree object

collapse_tree_to_level <- function(treeplot, level) {
  
  # TODO: Integrate this with the "scale_tree_to_level" function
  
  letter <- str_sub(level, 1, 1)
  pattern <- paste0(letter, "__")
  
  nodes <- treeplot$data |>
    filter(str_detect(taxon, pattern)) |>
    pull(node) |> 
    as.numeric()
  
  collapsed_treeplot <- treeplot
  
  for (node in nodes) {
    
    collapsed_treeplot <- collapse(
      collapsed_treeplot, 
      node, 
      mode = "mixed", 
      color = "black", 
      fill = NA)
    
  }
  
  collapsed_treeplot
  
}

#' Add interactive annotations to taxons, that shows on hover
#' the name of the taxon and its number of genomes
#'
#'
#' @param treeplot 
#' @param level 
#' @param metadata 
#'
#
annotate_taxon_nodes <- function(treeplot, level, metadata_df = metadata) {
  
  level_pattern <- paste0(str_sub(level, 1, 1), "__")
  taxon_pattern <- paste0("(", level_pattern, ".+?)(?=;|$)")
  
  nodes_df <- treeplot$data |>
    filter(str_detect(taxon, level_pattern)) |> 
    # Keep as taxon only whats after the level_pattern, and either a semicolon or EOL
    mutate(taxon = str_match(taxon, taxon_pattern)[,2], 
           n_genomes = NA_integer_)
  
  tax <- metadata_df$gtdb_taxonomy
  
  for (i in 1:nrow(nodes_df)) {
    taxon <- nodes_df[i,]$taxon
    taxon_pattern <- paste0(taxon, "(?:;|$)") 
    # Fill in n_genomes with count of matches
    nodes_df[i, "n_genomes"] <- sum(str_detect(tax, taxon))
  }
  
  clade_positions <- map_dfr(nodes_df$node, ~get_clade_position(treeplot, .x))
  
  nodes_df <- bind_cols(nodes_df, clade_positions)
  
  treeplot +
    geom_rect_interactive(
      data = nodes_df,
      aes(xmin = xmin, 
          xmax = xmax, 
          ymin = ymin, 
          ymax = ymax, 
          tooltip = paste(taxon, "\nn =", n_genomes)),
      color = NA,
      fill = NA
    ) 
}

add_tips <- function(treeplot) {
  
  tips <- treeplot$data[treeplot$data$isTip, ] |> 
    mutate(text = str_replace_all(gtdb_taxonomy, ";", "\n")) 
  
  treeplot + 
    geom_point_interactive(data = tips, 
               aes(x = x, y = y, tooltip = text), 
               size = 1) 
  
}

add_bootstraps <- function(treeplot) {
  treeplot <- treeplot +
    geom_point_interactive(
      data = filter(treeplot$data, ! is.na(bootstrap)),
      aes(x=x, y=y, color=bootstrap, tooltip=paste0(bootstrap, "%")), 
      show.legend = FALSE
    ) + scale_color_gradient2_interactive(
      low = "red", 
      mid = "yellow", 
      high = "green", 
      midpoint = 50
    ) 
  treeplot 
}

