

plot_stat_boxplot <-
  # TODO Add the possibilty to add a second variable 
  # that will be mapped to the aesthetic color of the points
  
  function(treeplot,
           level,
           stat = "genome_size",
           metadata_df = metadata,
           show_datapoints = FALSE,
           color_stat = "gc_content",
           show_quartiles = FALSE
           ) {
    # TODO: This is too complicated and slow,
    # I think metadata can just be subsetted to the level the tree was subsetted
    # This can be found out by looking at the taxonomy of the tree.
    
    level_pattern <- paste0(str_sub(level, 1, 1), "__")
    taxon_pattern <- paste0("(", level_pattern, ".+?)(?=;|$)")
    
    # Find all nodes at the selected level,
    # y will be the y coordinate of the boxplot
    
    taxa <- treeplot$data |>
      mutate(taxon = str_match(taxon, taxon_pattern)[, 2]) |>
      drop_na(taxon) |>
      select(taxon, y)
    
    all_taxa_pattern <- paste(taxa$taxon, collapse = "|")
    
    # Find all metadata datapoints in these taxa
    
    boxplot_data <- metadata_df |>
      filter(str_detect(gtdb_taxonomy, all_taxa_pattern)) |>
      mutate(taxon = str_match(gtdb_taxonomy, taxon_pattern)[, 2]) |>
      left_join(taxa, by = "taxon") |>
      mutate(tooltip_text = taxon)
    
    if (show_quartiles == TRUE) {
      boxplot_data <- boxplot_data |>
        group_by(taxon) |>
        mutate(q = list(quantile(.data[[stat]])) |>
                 map( ~ round(.x, 1)) |>
                 map_chr(
                   ~ glue::glue("
            Min:\t{.[1]}
            1st Q:\t{.[2]}
            Median:\t{.[3]}
            3rd Q:\t{.[4]}
            Max:\t{.[5]}")
                 ), tooltip_text = paste(taxon, "\n", q))
    }
    
    # Label
    
    stat_label <- str_to_title(str_replace_all(stat, "_", " "))
    
    # Plot
    
    boxplot <- ggplot(boxplot_data, ) +
      geom_boxplot_interactive(aes(
        x = .data[[stat]],
        y = y,
        group = taxon,
        tooltip = tooltip_text
      ), 
      outlier.alpha = 0.1, outlier.size = 1) +
      xlab(stat_label)
    
    if (show_datapoints == TRUE) {
      
      median = media(.data[[color_stat]])
      
      boxplot <- boxplot +
        geom_jitter_interactive(
          aes(
            x = .data[[stat]],
            y = y,
            color = .data[[color_stat]],
            tooltip = str_replace_all(gtdb_taxonomy, ";", "\n")
          ),
          height = 1,
          width = 0,
          alpha = 0.1
        ) + 
        scale_color_gradient2(
          low = "blue",
          mid = "yellow",
          high = "red", 
          midpoint = median)
    }
    
    return(boxplot)
    
  }

#' Plots the tree and the boxes side by side
#' forces them to have the same y axis
#' and forces them to have the same theme
#'
#' @param treeplot
#' @param boxplots
#'
#' @return a cowplot grid

plot_tree_and_boxplots <- function(treeplot, boxplots) {
  theme_plot <- theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank())
  
  range_y <- range(treeplot$data$y, na.rm = TRUE)
  
  coords <-
    coord_cartesian(ylim = range_y,
                    expand = FALSE,
                    clip = "off")
  breaks <- scale_y_continuous(breaks = seq(0, 2000, by = 5))
  
  grid <- plot_grid(treeplot + coords + breaks + theme_plot +
                      xlab("Substitutions per BP"),
                    boxplots + coords + breaks + theme_plot,
                    align = "h")
  grid
}
