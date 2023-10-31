download_data <- function(
    gtdb_root = "https://data.gtdb.ecogenomic.org/releases/release214/214.1/"
) {
  
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  tree_url <- paste0(gtdb_root, "bac120_r214.tree")
  tree_file <- file.path("data", "bac120_r214.tree")
  
  if (!file.exists(tree_file)) {
    download.file(tree_url, tree_file)
  }
  
  metadata_url <- paste0(gtdb_root, "bac120_metadata_r214.tsv.gz")
  metadata_gz <- file.path("data", "bac120_metadata_r214.tsv.gz")
  
  if (!file.exists(metadata_gz)) {
    download.file(metadata_url, metadata_gz, method = "wininet")
  }
  
}