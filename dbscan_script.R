#APD SINGLE INPUT
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
  
  
  sheet1<-read_xls(file.path(raw_file),sheet=1)
  sheet2<-read_xls(file.path(raw_file),sheet=2)
  sheet3<-read_xls(file.path(raw_file),sheet=3)
  sheet4<-read_xls(file.path(raw_file),sheet=4)
  sheet5<-read_xls(file.path(raw_file),sheet=5)
  sheet6<-read_xls(file.path(raw_file),sheet=6)
  
  ##CLUSTER AP VISUAL PLOT
  colnames(sheet4) <- c("Burst Number","AP Number",1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)
  clusterfigdata <- sheet4[-c(1,2)]
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
  sheet3$ap_id <- sheet3$`Burst Number`+(sheet3$`AP Number`*0.01)
  sheet4$ap_id <- sheet4$`Burst Number`+(sheet4$`AP Number`*0.01)
  all_ap <- merge(sheet3,sheet4)
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
  
  # Iterate through the list of data frames
  for (i in seq_along(split_clusters)) {
    # Remove outliers from the 'AP Latency (s)' column in the i-th data frame
    split_clusters[[i]] <- remove_outliers(split_clusters[[i]], 'AP Latency (s)')
  }
  ###AP PLOT PER CLUSTER
  clusterplotlist <- list()
  for (i in seq_along(split_clusters)) {
    df <- split_clusters[[i]]
    
    #MELT TIME
    sheet5_row2 <- c(sheet5[2,])
    numeric_vector <- as.numeric(sheet5_row2)
    max_y <- max(numeric_vector, na.rm = TRUE)
    max_y <- max_y/2
    df <- df[-c(1,2,4:9,11)]
    df_t <- t(df[3:34])
    df_t <- as.data.frame(t(df[3:34]))
    df_t$Average <- rowMeans(df_t)
    df_aptime <- list(1:32)
    df_plot <- cbind.data.frame(df_aptime,df_t)
    df_plot <- rename(df_plot,"ap_time"="1:32") 
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
  beats_total <- nrow(sheet6)
  burst_total <- max(sheet2$`Burst Number`)
  total_time <- as.numeric(max(sheet1$`Data Duration (s)`))
  #cluster cleaner
  cluster_prob_list <- list() 
  beats_prob_list <- list()
  timeline_list <- list()
  
  for (i in seq_along(split_clusters)) {
    cluster <- as.data.frame(split_clusters[[i]])
    cluster <- cluster[-c(12:44)]
    cluster <- rename(cluster,"ap_loc_sec"="AP Location from the begining of the Section (s)")
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
  multifire_prob <- grid.arrange(grobs = multifire_list, ncol = 4)
  
  ggsave("multifire_prob.png",multifire_prob,path = plots_folder,width=15,height=15)
  
  
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
    lat_amp_with_clusters <- rename(lat_amp_with_clusters,"neuron_id"="Cluster")
    lat_amp_with_clusters_list[[i]] <- lat_amp_with_clusters
    
    # Plot the clusters
    min_latency <- min(sheet3$`AP Latency (s)`,na.rm=TRUE)
    max_latency <- max(sheet3$`AP Latency (s)`,na.rm=TRUE)
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
  lat_amp_summary <- grid.arrange(grobs=cluster_lat_amp_list,ncol=4)
  ggsave("latency_amplitude.png",lat_amp_summary,path = plots_folder, height=15,width=15)
  
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
  ggsave("cluster_summary.png",cluster_summary,path = plots_folder,height = gridheight,width = 15)
  
  all_lat_amp <- sheet3[c(4,8,9)]
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
    dbcluster <- rename(dbcluster,"ap_loc_sec"="AP Location from the begining of the Section (s)")
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
  timeline_plot <- grid.arrange(grobs=timeline_list,ncol=1)
  timelength <- length(combined_grobs_list)
  ggsave("cluster_timeline.png",timeline_plot,path=plots_folder,height=timelength,width=30)
  #Amplitude sorted cluster plot
  
  
  
  
  # Combine all data frames into one
  neurons_df <- do.call(rbind, dbscan_split_clusters_list)
  neurons_df$neuron_cluster_id <- paste(neurons_df$`Cluster Number`, as.character(neurons_df$neuron_id), sep = "_")
  
  
  #BURST RRI CALC
  burst_rri <- sheet6[c(1,6)]
  burst_rri <- burst_rri %>% filter(`Burst numbers`!= 0)
  burst_rri <- rename(burst_rri,"Burst Number"="Burst numbers")
  rri_neuron_df <- merge(neurons_df,burst_rri)
  rri_neuron_df <- rename(rri_neuron_df,"ap_loc_sec"="AP Location from the begining of the Section (s)")
  burstsort_df <- rri_neuron_df[-c(12:43)]
  sort_amp <- sheet2
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
df_mean_ap_latency <- do.call(rbind, dbscan_split_clusters_list)
mean_ap_latency <- df_mean_ap_latency %>%
group_by(`Burst Number`,`Cluster Number`, neuron_id) %>%  # Group by Cluster Number and neuron_id
summarise(mean_ap_latency = mean(ap_latency, na.rm = TRUE),sd_ap_latency= sd(ap_latency, na.rm=T))  # Calculate the mean ap_latency
write.csv(mean_ap_latency,file.path(file_id_folder, paste0(file.id, "_apd_latency_summary.csv")))
  ###SAVE R FILE FOR FUTURE USE
  # Save the R session inside the folder
  save.image(file.path(file_id_folder, paste0(file.id, "_session.RData")))
  #save and clean environment for new session
  
  saveRDS(environment(), file.path(file_id_folder, paste0(file.id, "_environment.RDS")))
  
}}
print("DBSCAN ANALYZED")
