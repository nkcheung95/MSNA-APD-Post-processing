# CLUSTER ISI
### LIBLOAD

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

# Check if raw_files is not NULL and not empty
if (!is.null(raw_files) && length(raw_files) > 0) {
  # Iterate over the files and process them
  for (raw_file in raw_files) {
    file.id <- sub("\\.xls$", "", basename(raw_file))
    file.id <- sub("raw_data/", "", file.id)
    
    # Create a folder with the file.id name inside the analyzed folder
    file_id_folder <- file.path(analyzed_folder, file.id)
    dir.create(file_id_folder, recursive = TRUE)
    
    # Create a "plots" folder inside the file.id folder
    plots_folder <- file.path(file_id_folder, "plots")
    dir.create(plots_folder)
    
    print(file.id)
    
    # Get the number of sheets in the file
    sheets <- excel_sheets(path = raw_file)
    num_sheets <- length(sheets)
    
    # Conditional logic to read based on number of sheets
    if (num_sheets == 6) {
      summ_sheet <- read_xls(file.path(raw_file), sheet = 1)
      burst_sheet <- read_xls(file.path(raw_file), sheet = 2)
      ap_sheet <- read_xls(file.path(raw_file), sheet = 3)
      ap_shapes_sheet <- read_xls(file.path(raw_file), sheet = 4)
      clusters_sheet <- read_xls(file.path(raw_file), sheet = 5)
      rri_sheet <- read_xls(file.path(raw_file), sheet = 6)
      
    } else if (num_sheets == 4) {
      summ_sheet <- read_xls(file.path(raw_file), sheet = 1)
      ap_sheet <- read_xls(file.path(raw_file), sheet = 2)
      ap_shapes_sheet <- read_xls(file.path(raw_file), sheet = 3)
      clusters_sheet <- read_xls(file.path(raw_file), sheet = 4)
      
    } else {
      cat("Unexpected number of sheets in file:", raw_file, "\n")
    }

    all_ap <- merge(ap_sheet,ap_shapes_sheet)
    all_ap <- drop_na(all_ap)
    ##CLUSTER SEPARATOR
    ###BASELINE
    #Factor the clusters
    all_ap <- all_ap %>%
      arrange(all_ap$'Cluster Number')
    all_ap$`Cluster Number` <- as.factor(all_ap$`Cluster Number`)
    # Get unique values from the cluster column
    cluster_values <- unique(all_ap$`Cluster Number`)
    
    # Create a list to store the new dataframes
    split_clusters <- list()
    
    # Loop through unique values and create a dataframe for each value
    for (value in cluster_values) {
      subset_df <- all_ap[all_ap$`Cluster Number` == value, ]
      split_clusters[[value]] <- subset_df
    }
    
    
    
      }
}
