
download_and_unzip <- function(url) {
  
  # Extract the file name from the URL
  file_name <- basename(url)
  
  # Construct the local file path
  local_file_path <- file.path("data", file_name)
  
  # Check if the file exists, and download it if not
  if (!file.exists(local_file_path)) {
    cat("Downloading file from:", url, "\n")
    download.file(url, local_file_path, method = "auto")
  } else {
    cat("File already exists:", local_file_path, "\n")
  }
  
  # Check if the file is a ZIP archive and unzip it if needed
  if (endsWith(local_file_path, "gz")) {
    cat("Unzipping file:", file_name, "\n")
    unzip(local_file_path, exdir = "data")
  } else {
    cat("File is not a ZIP archive:", file_name, "\n")
  }
  
  cat("Process complete.\n")
}
