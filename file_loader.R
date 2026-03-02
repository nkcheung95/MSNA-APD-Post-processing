###LIBLOAD

# Define the packages you want to use
packages <- c(
  "dbscan", "cluster", "ggsci", "grid", "gridExtra", 
  "purrr", "readxl", "ggpubr", "rstatix",
  "stringr", "magick", "fs", "patchwork", "factoextra",
  "reshape","reshape2", "tidyverse","forcats","FNN","tcltk"
)

# Function to install and load packages
install_load_packages <- function(packages) {
  # Check which packages are not installed
  not_installed <- setdiff(packages, rownames(installed.packages()))
  
  # Install the missing packages
  if (length(not_installed) > 0) {
    install.packages(not_installed)
  }
  
  # Load all the packages
  invisible(sapply(packages, library, character.only = TRUE))
}

# Call the function to install and load packages
install_load_packages(packages)
wd <- getwd()
analyzed_folder <- "analyzed_data"

if (file.exists(analyzed_folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(analyzed_folder)
  
}
# Function to select multiple files using tcltk
select_files <- function() {
  tk_choose.files(multi = TRUE, filters = matrix(c("Excel Files", "*.xls"), ncol = 2))
}

# Call the function to get the file paths
raw_files <- select_files()


# Initialize an empty list to store all data
all_data <- list()

if (!is.null(raw_files) && length(raw_files) > 0) {
  
  for (raw_file in raw_files) {
    # 1. Generate unique File ID
    file_id <- sub("\\.xls$", "", basename(raw_file))
    
    # 2. Setup directory structure
    file_id_folder <- file.path(analyzed_folder, file_id)
    if (!dir.exists(file_id_folder)) dir.create(file_id_folder, recursive = TRUE)
    
    plots_folder <- file.path(file_id_folder, "plots")
    if (!dir.exists(plots_folder)) dir.create(plots_folder)
    
    message("Processing: ", file_id)

    # 3. Read sheets into a named list for this specific file
    # We use a list so we don't overwrite variables in each loop iteration
    file_sheets <- list(
      summ      = read_xls(raw_file, sheet = 1),
      burst     = read_xls(raw_file, sheet = 2),
      ap        = read_xls(raw_file, sheet = 3),
      ap_shapes = read_xls(raw_file, sheet = 4),
      clusters  = read_xls(raw_file, sheet = 5),
      rri       = read_xls(raw_file, sheet = 6)
    )
    
    # 4. Store this file's list into our master list using the file_id as the name
    all_data[[file_id]] <- file_sheets
  }
}

# Now you can access your data like this:
# all_data[["File_Name_1"]]$burst