#####AP DBSCAN mean_latency summary
df_mean_ap_latency <- do.call(rbind, dbscan_split_clusters_list)
mean_ap_latency <- test_dataframe %>%
group_by(`Burst Number`,`Cluster Number`, neuron_id) %>%  # Group by Cluster Number and neuron_id
summarise(mean_ap_latency = mean(ap_latency, na.rm = TRUE),sd_ap_latency= sd(ap_latency, na.rm=T))  # Calculate the mean ap_latency
write.csv(mean_ap_latency,file.path(file_id_folder, paste0(file.id, "_apd_latency_summary.csv")))
