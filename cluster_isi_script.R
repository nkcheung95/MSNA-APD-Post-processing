#CLUSTERISI
for (file.id in names(all_data)) {
  
  message("Analyzing: ", file.id)
  
  # Pull the specific sheets for THIS file into local variables 
  # so your existing analysis code doesn't need to be rewritten!
  summ_sheet      <- all_data[[file.id]]$summ
  burst_sheet     <- all_data[[file.id]]$burst
  ap_sheet        <- all_data[[file.id]]$ap
  ap_shapes_sheet <- all_data[[file.id]]$ap_shapes
  clusters_sheet  <- all_data[[file.id]]$clusters
  rri_sheet       <- all_data[[file.id]]$rri
  
  # --- Setup Folders ---
  file_id_folder <- file.path(analyzed_folder, file.id)
  if (!dir.exists(file_id_folder)) dir.create(file_id_folder, recursive = TRUE)
  plots_folder <- file.path(file_id_folder, "plots")
  if (!dir.exists(plots_folder)) dir.create(plots_folder)
    all_ap <- merge(ap_sheet, ap_shapes_sheet)
    all_ap <- drop_na(all_ap)
    
    if (!"Burst Number" %in% colnames(all_ap)) {
      all_ap$`Burst Number` <- 1
    }
    
    colnames(all_ap)[colnames(all_ap) %in% c("AP Location (s)", "AP Location from the begining of the Section (s)")] <- "ap_loc_sec"
        colnames(all_ap)[colnames(all_ap) %in% c("AP Location within burst (s)")] <- "ap_loc_sec_inburst"

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
        # Function to calculate differences and store relevant information from burst times
    calculate_diff_info_burst <- function(df) {
      df %>%
        # Group by 'Burst Number' and 'Cluster Number' to ensure calculations happen within these groups
        group_by(`Burst Number`, `Cluster Number`) %>%
        # Sort within each group by 'ap_loc_sec' to ensure time is in the correct order
        arrange(ap_loc_sec_inburst) %>%
        # Calculate the difference between adjacent values of 'ap_loc_sec'
        mutate(ap_isi = ap_loc_sec_inburst - lag(ap_loc_sec_inburst)) %>%
        # Remove NA values that arise from the lag function
        filter(!is.na(ap_isi)) %>%
        select(`Burst Number`, `Cluster Number`, ap_isi)
    }
    # Apply the function to each dataframe in the list
    cluster_differences <- lapply(split_clusters, calculate_diff_info_burst)
    
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
    cl_ap_isi_min <- min(cluster_data$ap_isi, na.rm = TRUE)
    cl_ap_isi_max <- max(cluster_data$ap_isi, na.rm = TRUE)
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
      
    output_data <- data.frame(file.id,cluster,cl_isi_count,cl_ap_isi_me,cl_ap_isi_sd,cl_ap_isi_md,cl_ap_isi_iqr,cl_ap_isi_min,cl_ap_isi_max,n_2fire,n_3fire,n_4fire,`n_5+fire`)
      
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

    ggsave((file.path(plots_folder,paste(file.id,"_isi_plot.png"))),plot=combined_plot, width = 6, height = (length(plots_list)*1.5))
    write.csv(isi_output,file.path(file_id_folder, paste(file.id,"_isi_summary.csv")))
   message("Cluster ISI analyzed for: ", file.id)
}

print("ALL FILES ANALYZED")