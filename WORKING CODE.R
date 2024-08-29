#####AP DBSCAN mean_latency summary
test_dataframe <- do.call(rbind, dbscan_split_clusters_list)
mean_ap_latency <- test_dataframe %>%
  +     group_by(burst_number,`Cluster Number`, neuron_id) %>%  # Group by Cluster Number and neuron_id
  +     summarise(mean_ap_latency = mean(ap_latency, na.rm = TRUE))  # Calculate the mean ap_latency
