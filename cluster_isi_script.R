# CLUSTER ISI
### LIBLOAD

# Define the packages you want to use
packages <- c(
  "dbscan", "cluster", "ggsci", "grid", "gridExtra", 
  "purrr", "readxl", "ggpubr", "rstatix",
  "stringr", "magick", "fs", "patchwork", "factoextra",
  "reshape", "reshape2", "tidyverse", "forcats", "FNN", "tcltk"
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
    
    all_ap <- merge(ap_sheet, ap_shapes_sheet)
    all_ap <- drop_na(all_ap)
    
    if (!"Burst Number" %in% colnames(all_ap)) {
      all_ap$`Burst Number` <- 1
    }
    
    colnames(all_ap)[colnames(all_ap) %in% c("AP Location (s)", "AP Location from the begining of the Section (s)")] <- "ap_loc_sec"
    
    ## CLUSTER SEPARATOR
    # Factor the clusters
    all_ap <- all_ap %>%
      arrange(all_ap$`Cluster Number`)
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
    
    # Function to calculate differences and store relevant information
    calculate_diff_info <- function(df) {
      df %>%
        # Group by 'Burst Number' and 'Cluster Number' to ensure calculations happen within these groups
        group_by(`Burst Number`, `Cluster Number`) %>%
        # Sort within each group by 'ap_loc_sec' to ensure time is in the correct order
        arrange(ap_loc_sec) %>%
        # Calculate the difference between adjacent values of 'ap_loc_sec'
        mutate(ap_isi = ap_loc_sec - lag(ap_loc_sec)) %>%
        # Remove NA values that arise from the lag function
        filter(!is.na(ap_isi)) %>%
        select(`Burst Number`, `Cluster Number`, ap_isi)
    }
    
    # Apply the function to each dataframe in the list
    cluster_differences <- lapply(split_clusters, calculate_diff_info)
    
    # Combine all differences into a single data frame
    all_differences <- bind_rows(cluster_differences, .id = "Cluster")
    
    # Calculate the mean of ap_isi across all clusters and bursts
    mean_ap_isi <- mean(all_differences$ap_isi, na.rm = TRUE)
    max_isi <- max(all_differences$ap_isi, na.rm = TRUE)
    
    # Create a list to store plots
    plots_list <- list()
    data_list <- list()
    # Get unique clusters
    unique_clusters <- unique(all_differences$Cluster)
    
    # Loop through each unique cluster
    for (cluster in unique_clusters) {
      # Filter data for the current cluster
      cluster_data <- all_differences %>% 
        filter(Cluster == cluster)
          # Calculate summary data mean/median of ap_isi for this cluster and bursts
    cl_ap_isi_md <- median(cluster_data$ap_isi, na.rm = TRUE)
    cl_ap_isi_me <- mean(cluster_data$ap_isi, na.rm = TRUE)
    cl_ap_isi_sd <- sd(cluster_data$ap_isi, na.rm = TRUE)
    cl_ap_isi_iqr <- IQR(cluster_data$ap_isi, na.rm = TRUE)
    cl_isi_count <- nrow(cluster_data)
multicount <- cluster_data %>%
  group_by(Cluster, `Burst Number`) %>%
  mutate(pair_count = n()) %>%
  ungroup() %>%
  mutate(
    n_2fire    = as.integer(pair_count == 1),
    n_3fire    = as.integer(pair_count == 2),
    n_4fire    = as.integer(pair_count == 3),
    `n_5+fire` = as.integer(pair_count >= 4)
  ) %>%
  select(-pair_count)

    n_2fire    <- sum(multicount$n_2fire)
    n_3fire    <- sum(multicount$n_3fire)
    n_4fire    <- sum(multicount$n_4fire)
    `n_5+fire` <- sum(multicount$`n_5+fire`)
      
    output_data <- data.frame(file.id,cluster,cl_isi_count,cl_ap_isi_me,cl_ap_isi_sd,cl_ap_isi_md,cl_ap_isi_iqr,n_2fire,n_3fire,n_4fire,`n_5+fire`)
      
      # Generate the plot for the current cluster
      p <- ggplot(cluster_data, aes(x = ap_isi, fill = as.factor(`Burst Number`), color = as.factor(`Burst Number`))) +
        # Overall cluster-level density plot with black line and grey shading
        geom_density(data = cluster_data, aes(x = ap_isi), color = "black", fill = "grey", alpha = 0.3, linewidth = 1) +
        # Add jittered points to show individual values
        geom_point(position = position_jitter(width = 0, height = 0.1), aes(y = 0), size = 2) +
        # Vertical red dashed line for mean_ap_isi
        geom_vline(xintercept = cl_ap_isi_md, linetype = "dashed", color = "red") +
        geom_vline(xintercept = cl_ap_isi_me, linetype = "solid", color = "blue") +
        xlim(0, max_isi) +  # Set x-axis limits
        labs(title = paste("Cluster", cluster)) +
        theme_classic2() +
        annotate("text", x=cl_ap_isi_md,y=3,label=paste("median",round(cl_ap_isi_md,2)))+
        annotate("text", x=cl_ap_isi_me,y=1,label=paste("mean",round(cl_ap_isi_me,2)))+
        theme(
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.y = element_blank(),    # Remove y-axis text
          legend.position = "none"           # Remove legends
        )
      
      # Add plot to the list
      plots_list[[cluster]] <- p
      data_list[[cluster]] <- output_data
    }
    
    # Create a text grob with the file ID
file_label <- textGrob(paste("File ID:", file.id), 
                       gp = gpar(fontsize = 10))

# Combine all plots + label into one vertical column
combined_plot <- do.call(grid.arrange, c(plots_list, list(file_label), ncol = 1))

    #dataframe for export
    isi_output <- bind_rows(data_list)

    ggsave(file.path(plots_folder,paste(file.id,"_isi_plot.png"),plot=combined_plot, width = 6, height = (length(plots_list)*1.5),)
    write.csv(isi_output,file.path(file_id_folder, paste(file.id,"_isi_summary.csv"))
  }
}
