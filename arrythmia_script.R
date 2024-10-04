# Define the packages you want to use
packages <- c(
  "cluster", "ggsci", "grid", "gridExtra", 
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
    
    
    sheet1<-read_xls(file.path(raw_file),sheet=1)#data description
    sheet2<-read_xls(file.path(raw_file),sheet=2)#ap descriptions
    sheet3<-read_xls(file.path(raw_file),sheet=3)#ap shape
    sheet4<-read_xls(file.path(raw_file),sheet=4)#cluster summary
    ##CLUSTER AP VISUAL PLOT
    colnames(sheet3) <- c("AP Number",1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)
    clusterfigdata <- sheet3[-c(1)]
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
    #merge
    all_ap <- merge(sheet2,sheet3)
    all_ap <- drop_na(all_ap)
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
      sheet4_row2 <- c(sheet4[2,])
      numeric_vector <- as.numeric(sheet4_row2)
      max_y <- max(numeric_vector, na.rm = TRUE)
      max_y <- max_y/2
      df <- df[-c(2:5,7)]
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
    
    ######Multifire???
    ###Cluster Latency/Amp

  #####LOOP END
}}