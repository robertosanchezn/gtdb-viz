tax_levels <- c(
  "domain",
  "phylum",
  "class",
  "order",
  "family",
  "genus",
  "species"
)

read_metadata <- function(connection) {
  metadata <- read_tsv(
    connection,
    col_select = c(
      accession,
      gtdb_genome_representative,
      coding_density,
      gc_percentage,
      genome_size,
      gtdb_taxonomy,
    ),
    col_types = "cdddc"
  ) |> 
    separate(gtdb_taxonomy, tax_levels, sep = ";", remove = FALSE) |> 
    mutate(across(all_of(tax_levels), ~str_sub(.x, 4)))
}


read_and_format_tree <- function(tree_file, metadata) {
  gtdb_tree <- read.tree(tree_file)
  
  # Remove single quotes
  
  gtdb_tree$node.label <- str_remove_all(gtdb_tree$node.label, "'")
  
  tax <- distinct(metadata, label = gtdb_genome_representative, gtdb_taxonomy)
  
  species_genome_count <- metadata |> 
    mutate(species = str_match(gtdb_taxonomy, "(s__.+?$)")[,2]) |>
    group_by(species) |>
    summarise(n_genomes = n()) |> 
    arrange(desc(n_genomes))
  
  gtdb_tree <- as_tibble(gtdb_tree) |>
    left_join(tax, by = "label") |> 
    mutate(
      # Split node labels into bootstrap and taxon
      bootstrap = str_match(label, "(^\\d{1,3}\\.\\d)(?=$|:)")[, 2],
      bootstrap = as.numeric(bootstrap),
      species = str_match(gtdb_taxonomy, "(s__.+?$)")[,2],
      taxon = str_match(label, "^\\d{1,3}\\.\\d:(.+$)")[, 2],
      taxon = if_else(is.na(taxon) & !is.na(species), species, taxon),
      label = if_else(is.na(gtdb_taxonomy), label, species)
    ) |>
    left_join(species_genome_count, by = "species") |>
    as.treedata()
  
  gtdb_tree
  
}