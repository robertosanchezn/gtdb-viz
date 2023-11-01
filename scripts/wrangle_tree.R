subset_tree_to_taxon <- function(tree = gtdb_tree, suffixed_taxon) {
  
  pattern <-paste0(suffixed_taxon, "(?:;|$)") 
  
  node <- tibble(as_tibble(tree)) |>
    filter(str_detect(taxon, pattern)) |>
    pull(node)
  
  subset_tree <- tree_subset(tree, node, levels_back = 0)
  
  subset_tree
  
}