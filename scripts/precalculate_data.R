get_n_genomes <- function(metadata = metadata) {
  tax_levels |> 
    set_names() |> 
    map(\(x) 
        metadata |>
          group_by(!!sym(x)) |>
          tally() |>
          arrange(desc(n))
    )
}

get_descendent_taxa <- function(
    metadata = metadata,
    taxon_level, 
    taxon, 
    desc_level
    ) {
  
  # If the letter is not under the level, throw an error
  
  level_letters <- str_sub(tax_levels, 1, 1)
  taxon_letter <- str_sub(taxon_level, 1, 1)
  desc_letter <- str_sub(desc_level, 1, 1)
  position_desc <- which(level_letters == desc_letter)
  position_taxon <- which(level_letters == taxon_letter)
  if (position_desc <= position_taxon) {
    stop("desc_level must be a level below taxon_level")
  }
  
  metadata |> 
    filter(!!sym(taxon_level) == taxon) |> 
    pull(!!sym(desc_level)) |>
    unique()
}

