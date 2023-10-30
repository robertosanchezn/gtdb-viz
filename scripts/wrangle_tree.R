subset_tree_to_taxon <- function(tree = gtdb_tree, level, taxon) {
  letter <- str_sub(level, 1, 1)
  pattern <-paste0(letter, "__", taxon, "(?:;|$)") 
  
  node <- as_tibble(tree) |>
    filter(str_detect(taxon, pattern)) |>
    pull(node)
  
  subset_tree <- tree_subset(tree, node, levels_back = 0)
  
  subset_tree
  
}