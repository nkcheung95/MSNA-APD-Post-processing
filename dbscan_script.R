#DBSCAN
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
  
  
  summ_sheet<-read_xls(file.path(raw_file),sheet=1)
  burst_sheet<-read_xls(file.path(raw_file),sheet=2)
  ap_sheet<-read_xls(file.path(raw_file),sheet=3)
  ap_shapes_sheet<-read_xls(file.path(raw_file),sheet=4)
  clusters_sheet<-read_xls(file.path(raw_file),sheet=5)
  rri_sheet<-read_xls(file.path(raw_file),sheet=6)
  
  ##CLUSTER AP VISUAL PLOT
  colnames(ap_shapes_sheet) <- c("Burst Number","AP Number",1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)
  clusterfigdata <- ap_shapes_sheet[-c(1,2)]
  clusterfigdata <- t(clusterfigdata)
  aptime <- (1:nrow(clusterfigdata))
  clusterfigdata <- cbind.data.frame(aptime,clusterfigdata)
  clusterfigdata_melt <- melt(clusterfigdata,id=c(aptime))
  clusterfigdata_melt <- clusterfigdata_melt[-c(2:32)]
  ap_visual <- ggplot(clusterfigdata_melt,aes(aptime,value))+
    geom_smooth()+
    theme_classic()+
    ggtitle("SMOOTHED AP")
  ap_visual
  
  #AP Identifier
  ap_sheet$ap_id <- ap_sheet$`Burst Number`+(ap_sheet$`AP Number`*0.01)
  ap_shapes_sheet$ap_id <- ap_shapes_sheet$`Burst Number`+(ap_shapes_sheet$`AP Number`*0.01)
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
  ###AP PLOT PER CLUSTER
  clusterplotlist <- list()
  for (i in seq_along(split_clusters)) {
    df <- split_clusters[[i]]
    
    #MELT TIME
    clusters_sheet_row2 <- c(clusters_sheet[2,])
    numeric_vector <- as.numeric(clusters_sheet_row2)
    max_y <- max(numeric_vector, na.rm = TRUE)
    max_y <- max_y/2
    df <- df[-c(1,2,4:9,11)]
    df_t <- t(df[3:34])
    df_t <- as.data.frame(t(df[3:34]))
    df_t$Average <- rowMeans(df_t)
    df_aptime <- list(1:32)
    df_plot <- cbind.data.frame(df_aptime,df_t)
    names(df_plot)[1] <- "ap_time"
    df_plot <- melt(df_plot,id=c("ap_time"))
    df_plotmean <- df_plot %>% filter(variable == 'Average')
    clusterplotlist[[i]] <- ggplot(df_plotmean,aes(ap_time,value,fill=ap_time))+
      geom_line()+
      ggtitle(paste("cluster", levels(df$`Cluster Number`)[i]))+
      ylim(-max_y,max_y)+
      ylab("")+
      xlab("")+
      guides(fill = "none")+
      theme_classic()+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
  }
  clustervis <- grid.arrange(grobs = clusterplotlist, ncol = 4)
  apshape_full <- grid.arrange(ap_visual,clustervis,ncol=2)
  ggsave("ap_plot.png", apshape_full, path = plots_folder,width = 15, height = 15)
  
  ###Firing probability
  beats_total <- nrow(rri_sheet)
  burst_total <- max(burst_sheet$`Burst Number`)
  total_time <- as.numeric(max(summ_sheet$`Data Duration (s)`))
  #cluster cleaner
  cluster_prob_list <- list() 
  beats_prob_list <- list()
  timeline_list <- list()
  
  for (i in seq_along(split_clusters)) {
    cluster <- as.data.frame(split_clusters[[i]])
    cluster <- cluster[-c(12:44)]
    names(cluster)[8] <- "ap_loc_sec"
    unique_bursts <- length(unique(cluster$`Burst Number`))
    beats_prob <- unique_bursts/beats_total*100
    offbeats_prob <- (beats_total-unique_bursts)/beats_total*100
    cluster_prob <- unique_bursts/burst_total*100
    off_prob <- (burst_total-unique_bursts)/burst_total*100
    beats_df <- cbind.data.frame(offbeats_prob,beats_prob)
    beats_df_melt <- melt(beats_df)
    prob_df <- cbind.data.frame(off_prob,cluster_prob)
    prob_df_melt <- melt(prob_df)
    cluster_prob_list[[i]] <- ggplot(prob_df_melt,aes(variable,value,fill=variable))+
      geom_col()+
      ggtitle(paste("cluster", levels(cluster$`Cluster Number`)[i]))+
      ylim(0,100)+
      ylab("Probability")+
      xlab("")+
      scale_fill_jco()+
      guides(fill = "none")+
      theme_classic()
    
    
    beats_prob_list[[i]] <- ggplot(beats_df_melt,aes(variable,value,fill=variable))+
      geom_col()+
      ggtitle(paste("cluster", levels(cluster$`Cluster Number`)[i]))+
      ylim(0,100)+
      ylab("Probability")+
      xlab("")+
      scale_fill_jco()+
      guides(fill = "none")+
      theme_classic()
    
    
    
  }
  
  cluster_prob <- grid.arrange(grobs = cluster_prob_list, ncol = 4)
  cluster_title <- textGrob("clusters", gp = gpar(fontsize = 16, fontface = "bold"))
  cluster_prob <- arrangeGrob(cluster_prob, top = cluster_title)
  
  ggsave("cluster_prob.png",cluster_prob,path = plots_folder,width=15,height=15)
  
  # Extract and reshape data from all plots
  beats_data <- beats_prob_list %>%
    imap_dfr(~ {
      .x$data %>% 
        mutate(cluster = .y) %>%  # .y is the list index (cluster number)
        select(cluster, variable, value)
    }) %>%
    pivot_wider(
      names_from = variable, 
      values_from = value,
      names_glue = "{variable}_prob"  # Rename columns (e.g., "on" -> "on_prob")
    ) %>%
    rename_with(~ gsub("_prob_prob", "_prob", .x))  # Fix double "_prob" if needed
  
  bursts_data <- cluster_prob_list %>%
    imap_dfr(~ {
      .x$data %>% 
        mutate(cluster = .y) %>%  # .y is the list index (cluster number)
        select(cluster, variable, value)
    }) %>%
    pivot_wider(
      names_from = variable, 
      values_from = value,
      names_glue = "{variable}_prob"  # Rename columns (e.g., "on" -> "on_prob")
    ) %>%
    rename_with(~ gsub("_prob_prob", "_prob", .x))  # Fix double "_prob" if needed

  prob_data <- merge(beats_data,bursts_data,by = "cluster")
  
  
  ###MULTIFIRE
  multifire_list<- list()
  for (i in seq_along(split_clusters)) {
    cluster <- as.data.frame(split_clusters[[i]])
    cluster <- cluster[-c(12:44)]
    # Count the occurrences of each numeric value
    cluster_counts <- table(cluster$`Burst Number`)
    
    # Create a summary table
    summary_table <- table(factor(cluster_counts, levels = 1:6, labels = c("1", "2", "3", "4", "5", "6+")))
    # Convert the summary table to a data frame
    multifire_df <- as.data.frame(summary_table)
    
    # Rename the columns
    colnames(multifire_df) <- c("Multiple_firings", "Frequency")
    freq_total <- sum(multifire_df$Frequency)
    multifire_df$Probability <- multifire_df$Frequency/freq_total*100
    
    multifire_list[[i]] <- ggplot(multifire_df,aes(Multiple_firings,Probability,fill = Multiple_firings))+
      geom_col()+
      ggtitle(paste("cluster", levels(cluster$`Cluster Number`)[i]))+
      ylim(0,100)+
      scale_fill_jco()+
      guides(fill="none")+
      theme_classic()
  }
  # Step 1: Extract data from each ggplot and combine
  multifire_data <- multifire_list %>%
    imap_dfr(~ {
      .x$data %>%
        mutate(cluster = .y) %>%  # .y = cluster number (list index)
        select(cluster, Multiple_firings, Frequency, Probability)
    }) %>%
    pivot_wider(
      names_from = Multiple_firings,
      values_from = c(Frequency, Probability),
      names_glue = "{.value}_{Multiple_firings}"  # Format: "frequency_1", "probability_1", etc.
    )
  prob_data <- merge(prob_data,multifire_data,by="cluster")
  
  # Step 2: Clean column names (optional, if needed)
multifire_data <- multifire_data %>%
    rename_with(~ gsub("frequency_", "freq_", .x))  # Rename "frequency_1" to "freq_1"
  
  multifire_prob <- grid.arrange(grobs = multifire_list, ncol = 4)
  
  ggsave("multifire_prob.png",multifire_prob,path = plots_folder,width=15,height=15)
  write.csv(prob_data,file.path(file_id_folder, paste0(file.id, "_probabilities.csv")))
  
  
  ###Cluster Latency/Amp
  
  # Initialize an empty list to store the cluster plots
  cluster_lat_amp_list <- list()
  
  # Initialize an empty list to store scaled data
  scaled_data_list <- list()
  lat_amp_with_clusters_list <- list()
  
  for (i in seq_along(split_clusters)) {
    lat_amp <- as.data.frame(split_clusters[[i]])
    lat_amp <- lat_amp[c(1, 3, 5, 9)]
    colnames(lat_amp) <- c("burst_number", "ap_id", "ap_amp", "ap_latency")
    
    # Perform scaling
    scaled_data <- scale(as.matrix(lat_amp$ap_latency))
    
    # Check for NaN values in scaled data
    scaled_data[is.nan(scaled_data)] <- 0
    
    # Store scaled data in list
    scaled_data_list[[i]] <- scaled_data
    
    # Perform DBSCAN clustering
    eps <- 0.7  # Adjust the epsilon parameter as needed
    minPts <- 4 # Adjust the minPts parameter as needed
    dbscan_result <- dbscan(scaled_data_list[[i]], eps = eps, minPts = minPts)
    
    # Extract cluster assignments
    cluster_assignments <- dbscan_result$cluster
    
    # Combine lat_amp and cluster_assignments
    
    lat_amp_with_clusters <- cbind(lat_amp, Cluster = factor(cluster_assignments))
    names(lat_amp_with_clusters)[5] <- "neuron_id"
    lat_amp_with_clusters_list[[i]] <- lat_amp_with_clusters
    
    # Plot the clusters
    min_latency <- min(ap_sheet$`AP Latency (s)`,na.rm=TRUE)
    max_latency <- max(ap_sheet$`AP Latency (s)`,na.rm=TRUE)
    cols <- c("0" = "#868686FF", "1" = "#0073C2FF", "2" = "#EFC000FF", "3" = "#CD534CFF")
    cluster_lat_amp_list[[i]] <-  ggplot(lat_amp_with_clusters, aes(x = ap_latency, y = ap_amp, color = neuron_id)) +
      geom_point() +
      geom_density(inherit.aes=FALSE,data=lat_amp_with_clusters,aes(ap_latency),adjust=0.5,alpha = 0.5)+
      labs(x = "Latency",
           y = "Amplitude",
           color = "DBSCAN") +
      xlim(min_latency,max_latency)+
      ggtitle(paste("cluster", levels(cluster$`Cluster Number`)[i]))+
      scale_color_manual(values=cols)+
      theme_classic()
    
  }
  lat_amp_summary <- grid.arrange(grobs=cluster_lat_amp_list,ncol=1)
  
  
  ###PLOT COMBINE 
  layout_matrix <- rbind(c(1, 2, 3, 3))
  combined_grobs_list <- list()
  
  for (i in seq_along(clusterplotlist)) {
    combined_plot <- grid.arrange(clusterplotlist[[i]], beats_prob_list[[i]],
                                  cluster_prob_list[[i]], multifire_list[[i]], 
                                  cluster_lat_amp_list[[i]], widths = c(1,1,1,2,2),ncol = 5)
    combined_grobs_list <- c(combined_grobs_list, list(combined_plot))
  }
  
  # Display the list of combined grobs
  print(combined_grobs_list)
  grid <- grid.arrange(grobs = combined_grobs_list,ncol=1)
  cluster_summary <- arrangeGrob(grid, top = file.id)
  gridheight <- (length(combined_grobs_list)*2)
  lat_amp_summary <- arrangeGrob(lat_amp_summary, top=file.id)
  ggsave("cluster_summary.png",cluster_summary,path = plots_folder,height = gridheight,width = 15,limitsize = FALSE)
  ggsave("latency_amplitude.png",lat_amp_summary,path = plots_folder, height=gridheight,width=15,limitsize=FALSE)
  all_lat_amp <- ap_sheet[c(4,8,9)]
  all_lat_amp <- drop_na(all_lat_amp)
  colnames(all_lat_amp) <- c("ap_amp","ap_latency","Cluster")
  all_lat_amp$Cluster <- as.integer(all_lat_amp$Cluster)
  all_lat_amp_plot <- ggplot(all_lat_amp,aes(ap_latency,ap_amp,color=Cluster))+
    geom_point() +
    labs(x = "Latency",
         y = "Amplitude",
         color = "Cluster") +
    xlim(min_latency,max_latency)+
    ggtitle("Amplitude/Latency")+
    scale_color_continuous(type = "viridis")+
    theme_classic()
  ggsave("all_lat_amp_plot.png",all_lat_amp_plot,path=plots_folder,height=15,width=15)
  #Neuron isolation
  # Initialize an empty list to store the merged data frames
  dbscan_split_clusters_list <- list()
  
  for (i in seq_along(split_clusters)) {
    # Merge the i-th data frame in split_clusters with the i-th data frame in lat_amp_with_clusters_list
    merged_data <- merge(split_clusters[[i]], lat_amp_with_clusters_list[[i]], by = "ap_id")
    
    # Store the merged data frame in the list
    dbscan_split_clusters_list[[i]] <- merged_data
  }
  
  # Iterate through the list of data frames
  for (i in seq_along(dbscan_split_clusters_list)) {
    # Convert 'neuron_id' column to character or numeric
    dbscan_split_clusters_list[[i]]$neuron_id <- as.character(dbscan_split_clusters_list[[i]]$neuron_id)
    # Change all 0 to 1 in the 'neuron_id' column for the i-th data frame
    dbscan_split_clusters_list[[i]][dbscan_split_clusters_list[[i]]$neuron_id == "0", "neuron_id"] <- "1"
    # If you need 'neuron_id' to be a factor again, you can convert it back
    dbscan_split_clusters_list[[i]]$neuron_id <- as.factor(dbscan_split_clusters_list[[i]]$neuron_id)
    #Cluster Timeline
    dbcluster <- dbscan_split_clusters_list[[i]]
    names(dbcluster)[8] <- "ap_loc_sec"
    timeline_list[[i]] <- ggplot(dbcluster, aes(ap_loc_sec,'Cluster Number',color=neuron_id))+
      geom_vline(xintercept = dbcluster$ap_loc_sec,color=dbcluster$neuron_id,linewidth=0.5)+
      xlim(0,total_time)+
      guides(fill = "none")+
      scale_color_jco()+
      theme_classic()+
      theme(axis.ticks = element_blank(),axis.text = element_blank(),
            panel.border = element_rect(color = "black", fill = NA))+
      ylab(levels(cluster$`Cluster Number`)[i])+
      xlab("")
    
  }
  #timeline_plot <- grid.arrange(grobs=timeline_list,ncol=1)
  #timelength <- length(combined_grobs_list)
  #ggsave("cluster_timeline.png",timeline_plot,path=plots_folder,height=timelength,width=30)
  #Amplitude sorted cluster plot
  
  
  
  
  # Combine all data frames into one
  neurons_df <- do.call(rbind, dbscan_split_clusters_list)
  neurons_df$neuron_cluster_id <- paste(neurons_df$`Cluster Number`, as.character(neurons_df$neuron_id), sep = "_")
  
  
  #BURST RRI CALC
  burst_rri <- rri_sheet[c(1,6)]
  burst_rri <- burst_rri %>% filter(`Burst numbers`!= 0)
  names(burst_rri)[1] <- "Burst Number"
  rri_neuron_df <- merge(neurons_df,burst_rri)
  names(rri_neuron_df)[8] <- "ap_loc_sec"
  burstsort_df <- rri_neuron_df[-c(12:43)]
  sort_amp <- burst_sheet
  sort_amp$sort <- order(sort_amp$`Burst Amp`)
  burstsort_df <- merge(burstsort_df,sort_amp)
  
  #Factor the clusters
  burstsort_df <- burstsort_df %>%
    arrange(burstsort_df$'Cluster Number')
  burstsort_df$`Cluster Number` <- as.factor(burstsort_df$`Cluster Number`)
  # Get unique values from the cluster column
  sort_cluster_values <- unique(burstsort_df$`Cluster Number`)
  
  # Create a list to store the new dataframes
  sort_split_clusters <- list()
  
  # Loop through unique values and create a dataframe for each value
  for (value in sort_cluster_values) {
    subset_df <- burstsort_df[burstsort_df$`Cluster Number` == value, ]
    sort_split_clusters[[value]] <- subset_df
  }
  
  #####AP DBSCAN mean_latency summary
  df_mean_ap_latency <- bind_rows(dbscan_split_clusters_list, .id = "cluster_name")
locator_ap_latency <- df_mean_ap_latency %>%
group_by(`Burst Number`,cluster_name, neuron_id) %>%  # Group by Cluster Number and neuron_id
summarise(locator_ap_latency = mean(ap_latency, na.rm = TRUE),sd_ap_latency= sd(ap_latency, na.rm=T))  # Calculate the mean ap_latency
write.csv(locator_ap_latency,file.path(file_id_folder, paste0(file.id, "LOCATOR_AP.csv")))


mean_ap_latency <- df_mean_ap_latency  %>%
  group_by(cluster_name) %>%
  summarise(across(
    c(ap_latency, ap_amp),
    list(
      median = ~median(., na.rm = TRUE),
      iqr = ~IQR(., na.rm = TRUE),
      q1 = ~quantile(., 0.25, na.rm = TRUE),
      q3 = ~quantile(., 0.75, na.rm = TRUE)
    )
  ))

write.csv(mean_ap_latency,file.path(file_id_folder, paste0(file.id, "_apd_latency_summary.csv")))


#####SEPARATE DBSCAN CLUSTERS
dbcluster_prob_list <- list()
dbbeats_prob_list <- list()
split_list_by_neuron <- function(original_list) {
  # Initialize empty list for results
  result_list <- list()
  
  # Process each element
  walk(seq_along(original_list), function(i) {
    item <- original_list[[i]]
    
    if ("neuron_id" %in% names(item) && is.factor(item$neuron_id)) {
      # Split into level 1 and level 2
      level1 <- item %>% filter(neuron_id != 2)
      level2 <- item %>% filter(neuron_id == 2)
      
      # Add level 1 to original position
      result_list[[as.character(i)]] <<- level1
      
      # Add level 2 to new position if it exists
      if (nrow(level2) > 0) {
        result_list[[paste0(i, "_2")]] <<- level2
      }
    } else {
      # Keep unchanged if no neuron_id or not factor
      result_list[[as.character(i)]] <<- item
    }
  })
  
  return(result_list)
}

# Usage:
dbprocessed_list <- split_list_by_neuron(dbscan_split_clusters_list)

# Initialize your lists with the same names as dbprocessed_list
dbcluster_prob_list <- vector("list", length(dbprocessed_list))
names(dbcluster_prob_list) <- names(dbprocessed_list)

dbbeats_prob_list <- vector("list", length(dbprocessed_list))
names(dbbeats_prob_list) <- names(dbprocessed_list)

for (i in seq_along(dbprocessed_list)) {
  cluster <- as.data.frame(dbprocessed_list[[i]])
  cluster <- cluster[-c(12:44)]
  names(cluster)[8] <- "ap_loc_sec"
  
  unique_bursts <- length(unique(cluster$`Burst Number`))
  beats_prob <- unique_bursts/beats_total*100
  offbeats_prob <- (beats_total-unique_bursts)/beats_total*100
  cluster_prob <- unique_bursts/burst_total*100
  off_prob <- (burst_total-unique_bursts)/burst_total*100
  
  beats_df <- cbind.data.frame(offbeats_prob, beats_prob)
  beats_df_melt <- melt(beats_df)
  prob_df <- cbind.data.frame(off_prob, cluster_prob)
  prob_df_melt <- melt(prob_df)
  
  # Use the original name from dbprocessed_list for the title
  original_name <- names(dbprocessed_list)[i]
  
  dbcluster_prob_list[[i]] <- ggplot(prob_df_melt, aes(variable, value, fill=variable)) +
    geom_col() +
    ggtitle(paste("cluster", original_name)) +  # Use original name here
    ylim(0, 100) +
    ylab("Probability") +
    xlab("") +
    scale_fill_jco() +
    guides(fill = "none") +
    theme_classic()
  
  dbbeats_prob_list[[i]] <- ggplot(beats_df_melt, aes(variable, value, fill=variable)) +
    geom_col() +
    ggtitle(paste("cluster", original_name)) +  # Use original name here
    ylim(0, 100) +
    ylab("Probability") +
    xlab("") +
    scale_fill_jco() +
    guides(fill = "none") +
    theme_classic()
}
# Extract and reshape data from all plots
dbbeats_data <- dbbeats_prob_list %>%
  imap_dfr(~ {
    .x$data %>% 
      mutate(cluster = .y) %>%  # .y is the list index (cluster number)
      select(cluster, variable, value)
  }) %>%
  pivot_wider(
    names_from = variable, 
    values_from = value,
    names_glue = "{variable}_prob"  # Rename columns (e.g., "on" -> "on_prob")
  ) %>%
  rename_with(~ gsub("_prob_prob", "_prob", .x))  # Fix double "_prob" if needed

dbbursts_data <- dbcluster_prob_list %>%
  imap_dfr(~ {
    .x$data %>% 
      mutate(cluster = .y) %>%  # .y is the list index (cluster number)
      select(cluster, variable, value)
  }) %>%
  pivot_wider(
    names_from = variable, 
    values_from = value,
    names_glue = "{variable}_prob"  # Rename columns (e.g., "on" -> "on_prob")
  ) %>%
  rename_with(~ gsub("_prob_prob", "_prob", .x))  # Fix double "_prob" if needed

dbprob_data <- merge(dbbeats_data,dbbursts_data,by = "cluster")

# Initialize the list with names from list
dbmultifire_list <- vector("list", length(dbprocessed_list))
names(dbmultifire_list) <- names(dbprocessed_list)

for (i in seq_along(dbprocessed_list)) {
  cluster <- as.data.frame(dbprocessed_list[[i]])
  cluster <- cluster[-c(12:44)]
  
  # Count the occurrences of each numeric value
  cluster_counts <- table(cluster$`Burst Number`)
  
  # Create a summary table
  summary_table <- table(factor(cluster_counts, levels = 1:6, labels = c("1", "2", "3", "4", "5", "6+")))
  
  # Convert the summary table to a data frame
  dbmultifire_df <- as.data.frame(summary_table)
  
  # Rename the columns
  colnames(dbmultifire_df) <- c("Multiple_firings", "Frequency")
  freq_total <- sum(dbmultifire_df$Frequency)
  dbmultifire_df$Probability <- dbmultifire_df$Frequency/freq_total*100
  
  # Get the original cluster name
  original_name <- names(dbscan_split_clusters_list)[i]
  
  dbmultifire_list[[i]] <- ggplot(dbmultifire_df, aes(Multiple_firings, Probability, fill = Multiple_firings)) +
    geom_col() +
    ggtitle(paste("cluster", original_name)) +  # Use original name here
    ylim(0, 100) +
    scale_fill_jco() +
    guides(fill = "none") +
    theme_classic()
}

# Step 1: Extract data from each ggplot and combine
dbmultifire_data <- dbmultifire_list %>%
  imap_dfr(~ {
    .x$data %>%
      mutate(cluster = .y) %>%  # .y will now be the original name
      select(cluster, Multiple_firings, Frequency, Probability)
  }) %>%
  pivot_wider(
    names_from = Multiple_firings,
    values_from = c(Frequency, Probability),
    names_glue = "{.value}_{Multiple_firings}"
  )

dbprob_data <- merge(dbprob_data,dbmultifire_data,by="cluster")
write.csv(dbprob_data,file.path(file_id_folder, paste0(file.id, "DBSCAN_probabilities.csv")))
#####AP DBSCAN mean_latency summary
df_dbmean_ap_latency <- bind_rows(dbprocessed_list, .id = "cluster_name")
dbmean_ap_latency <- df_dbmean_ap_latency  %>%
  group_by(cluster_name) %>%
  summarise(across(
    c(ap_latency, ap_amp),
    list(
      median = ~median(., na.rm = TRUE),
      iqr = ~IQR(., na.rm = TRUE),
      q1 = ~quantile(., 0.25, na.rm = TRUE),
      q3 = ~quantile(., 0.75, na.rm = TRUE)
    )
  ))

 write.csv(dbmean_ap_latency,file.path(file_id_folder, paste0(file.id, "DBSCAN_apd_latency_summary.csv")))


##### Cluster normalizer using reference 'normal' value for percentiles
clusters_amp <- data.frame(cluster_amplitude = unlist(clusters_sheet_row2))
clusters_lat_sheet <- c(clusters_sheet[3,])
clusters_lat <- data.frame(cluster_latency = unlist(clusters_lat_sheet))
clusters_desc <- cbind.data.frame(clusters_amp, clusters_lat)
# Check and convert columns to numeric if needed
clusters_desc$cluster_amplitude <- as.numeric(clusters_desc$cluster_amplitude)
clusters_desc$cluster_latency <- as.numeric(clusters_desc$cluster_latency)
# Extract min and max from clusters_sheet
n <- ncol(clusters_sheet)

# Extract numbers from the cell
extract_numbers <- function(text) {
  numbers <- str_extract_all(text, "\\d+\\.\\d+")[[1]]
  return(as.numeric(numbers))
}

# For normal_min (first number in parentheses)
normal_min_text <- clusters_sheet[5, 1]
normal_min <- extract_numbers(normal_min_text)[1]  # First number

# For normal_max (second number in parentheses)
normal_max_text <- clusters_sheet[5, n]
normal_max <- extract_numbers(normal_max_text)[2]  # Second number


# Normalize cluster data into 10 percentile bins based on reference 'normal' value
normalize_clusters <- function(clusters_desc, normal,normal_min) {
  # Ensure we have at least 10 clusters to avoid empty bins
  if (nrow(clusters_desc) < 10) {
    warning("Fewer than 10 clusters - some bins may be empty or contain few points")
  }
  
  # Create percentile breaks based on the reference 'normal' value (0% to 100% of normal)
  percentile_breaks <- seq(normal_min, normal, length.out = 11)
  
  # Bin the amplitudes according to the reference breaks
  clusters_desc$percentile_bin <- cut(
    clusters_desc$cluster_amplitude,
    breaks = percentile_breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  # Calculate mean amplitude and latency per bin
  result <- aggregate(cbind(cluster_amplitude, cluster_latency) ~ percentile_bin,
                      data = clusters_desc, 
                      FUN = median)
  
  # Add percentile range labels (relative to normal)
  result$percentile_range <- paste0((result$percentile_bin-1)*10, "-", result$percentile_bin*10, "%")
  
  # Order by bin and clean up output
  result <- result[order(result$percentile_bin), 
                   c("percentile_bin", "percentile_range", "cluster_amplitude", "cluster_latency")]
  
  return(result)
}

binned_clusters <- normalize_clusters(clusters_desc, normal_max,normal_min)
write.csv(binned_clusters,file.path(file_id_folder, paste0(file.id, "NORMALIZED_cluster_description.csv")))



#####SAVE R FILE FOR FUTURE USE
  # Save the R session inside the folder
  save.image(file.path(file_id_folder, paste0(file.id, "_session.RData")))
  #save and clean environment for new session
  
  saveRDS(environment(), file.path(file_id_folder, paste0(file.id, "_environment.RDS")))
  
}}
print("DBSCAN ANALYZED")
