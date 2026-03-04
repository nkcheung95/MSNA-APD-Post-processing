###DBSCAN###
###LIBRARIES###
#loaded via github loading call
#--------------------------------------------------FUNCTIONS-------

# get_github_version------------------------------------------------
# GitHub Script Version Fetcher
# Fn: Fetches and MD5-hashes raw script from GitHub to detect updates
# Exec: get_github_version(github_raw_url)
# ---------------------------------------------------------
get_github_version <- function(github_raw_url) {
  tryCatch({
    hash <- readLines(github_raw_url, warn = FALSE)
    digest::digest(hash, algo = "md5")
  }, error = function(e) {
    message("Warning: Could not fetch GitHub version, defaulting to offline check.")
    return(NULL)
  })
}

# is_already_analyzed---------------------------------------------------------
# Analysis Completion & Version Checker
# Fn: Checks if file has been analyzed with current script version, skips or re-runs accordingly
# Exec: is_already_analyzed(analyzed_folder, file_id, file_name, current_version)
#--------------------------------------------------------------------------
is_already_analyzed <- function(analyzed_folder, file_id, file_name, current_version) {
  file_id_folder <- file.path(analyzed_folder, file_id)
  master_csv     <- file.path(file_id_folder, paste0(file_name, "_master_summary.csv"))
  version_file   <- file.path(file_id_folder, "analysis_version.txt")

  if (!file.exists(master_csv)) return(FALSE)

  # If version check is unavailable, fall back to existence check only
  if (is.null(current_version)) {
    message("Skipping (already analyzed, version unknown): ", file_name)
    return(TRUE)
  }

  # Re-run if version file missing or version has changed
  if (!file.exists(version_file)) return(FALSE)
  stored_version <- readLines(version_file, warn = FALSE)
  if (stored_version != current_version) {
    message("Re-running (script updated on GitHub): ", file_name)
    return(FALSE)
  }

  message("Skipping (already analyzed, version current): ", file_name)
  return(TRUE)
}
# prep_apd_data---------------------------------------------------------
# Cluster split
# Fn: Prepare data and split into cluster list
# Exec: cluster_list <- prep_apd_data(ap_sheet, ap_shapes_sheet)
# ---------------------------------------------------------
prep_apd_data <- function(ap_sheet, ap_shapes_sheet) {
  
  # 1. Rename ap_shapes_sheet columns to match original (required for all downstream shape indexing)
  colnames(ap_shapes_sheet) <- c("Burst Number", "AP Number", 1:32)
  
  # 2. Create unique ID and merge
  ap_sheet$ap_id        <- ap_sheet$`Burst Number` + (ap_sheet$`AP Number` * 0.01)
  ap_shapes_sheet$ap_id <- ap_shapes_sheet$`Burst Number` + (ap_shapes_sheet$`AP Number` * 0.01)
  
  all_ap <- merge(ap_sheet, ap_shapes_sheet) |> tidyr::drop_na()
  all_ap <- all_ap %>% arrange(all_ap$`Cluster Number`)
  all_ap$`Cluster Number` <- as.factor(all_ap$`Cluster Number`)
  
  # 3. Return split list (preserves factor levels, matching original behaviour)
  return(split(all_ap, all_ap$`Cluster Number`))
}

# normalize_neurons_full---------------------------------------------------------
# Full Neuron Normalizer (Dual Output Version)
# Fn: Normalizes every AP relative to reference 'Normal' range & saves states
# Exec: norm_bundle <- normalize_neurons_full(neurons_df, clusters_sheet, plots_folder, file.id)
# ---------------------------------------------------------
normalize_neurons_full <- function(neurons_df, clusters_sheet, save_path, file_id) {
  
  # 1. Extraction Logic
  extract_numbers <- function(text) {
    numbers <- stringr::str_extract_all(text, "\\d+\\.\\d+")[[1]]
    return(as.numeric(numbers))
  }
  
  n <- ncol(clusters_sheet)
  
  # Reference Bounds
  ref_amp_min <- extract_numbers(clusters_sheet[5, 1])[1]
  ref_amp_max <- extract_numbers(clusters_sheet[5, n])[2]

# 2. Apply Normalization
  neurons_df_norm <- neurons_df %>%
    mutate(
      norm_amp_percent = ((ap_amp - ref_amp_min) / (ref_amp_max - ref_amp_min)) * 100
    )

  # Cap 0-100
  neurons_df_norm$norm_amp_percent <- pmax(0, pmin(100, neurons_df_norm$norm_amp_percent))

  # 3. Bin into 10% intervals
  neurons_df_norm$amp_percentile_bin <- cut(
    neurons_df_norm$norm_amp_percent,
    breaks         = seq(0, 100, by = 10),
    include.lowest = TRUE,
    labels         = paste0(seq(0, 90, by = 10), "-", seq(10, 100, by = 10), "%")
  )
  return(list(raw = neurons_df, normalized = neurons_df_norm))
}

# generate_smoothed_ap---------------------------------------------------------
# AP Smoother
# Fn: Generate smoothed AP figure
# Exec: ap_visual <- generate_smoothed_ap(ap_shapes_sheet)
# NOTE: ap_shapes_sheet must already have columns renamed (done inside prep_apd_data)
# ---------------------------------------------------------
generate_smoothed_ap <- function(ap_shapes_sheet) {
  
  # Rename in case called independently; safe to call again as names are already set by prep_apd_data
  colnames(ap_shapes_sheet) <- c("Burst Number", "AP Number", 1:32)
  
  clusterfigdata <- t(ap_shapes_sheet[-c(1, 2)])
  aptime <- 1:nrow(clusterfigdata)
  clusterfigdata <- cbind.data.frame(aptime, clusterfigdata)
  
  clusterfigdata_melt <- reshape2::melt(clusterfigdata, id = c("aptime"))
  # Drop the 'variable' column (factor of AP indices) — keep only aptime + value for geom_smooth
  clusterfigdata_melt <- clusterfigdata_melt[, c("aptime", "value")]
  
  ap_visual <- ggplot2::ggplot(clusterfigdata_melt, ggplot2::aes(aptime, value)) +
    ggplot2::geom_smooth() +
    ggplot2::theme_classic() +
    ggplot2::ggtitle("SMOOTHED AP")
  
  return(ap_visual)
}

# visualize_apd_clusters ---------------------------------------------------------
# Cluster plotter
# Fn: Generate and save the composite cluster plot
# Exec: shape_plots <- visualize_apd_clusters(cluster_list, main_vis, clusters_sheet, plots_folder)
# ---------------------------------------------------------
visualize_apd_clusters <- function(cluster_list, main_ap_vis, clusters_sheet, save_path) {
  
  # 1. Setup Y limits — matches original: max of row 2 / 2
  clusters_sheet_row2 <- c(clusters_sheet[2, ])
  numeric_vector <- as.numeric(clusters_sheet_row2)
  max_y <- max(numeric_vector, na.rm = TRUE) / 2
  
  # 2. Map plotting logic over every cluster
  cluster_plots <- lapply(seq_along(cluster_list), function(i) {
    df <- cluster_list[[i]]
    
    # Isolate the 32 shape columns by name (columns named 1:32, as set during prep_apd_data)
    shape_cols <- as.character(1:32)
    df_t <- as.data.frame(t(df[, shape_cols]))
    df_t[] <- lapply(df_t, as.numeric)
    df_t$Average <- rowMeans(df_t, na.rm = TRUE)
    
    df_aptime <- list(1:32)
    df_plot <- cbind.data.frame(df_aptime, df_t)
    names(df_plot)[1] <- "ap_time"
    df_plot <- reshape2::melt(df_plot, id = c("ap_time"))
    df_plotmean <- df_plot %>% dplyr::filter(variable == "Average")
    
    ggplot2::ggplot(df_plotmean, ggplot2::aes(ap_time, value, fill = ap_time)) +
      ggplot2::geom_line() +
      ggplot2::ggtitle(paste("cluster", levels(df$`Cluster Number`)[i])) +
      ggplot2::ylim(-max_y, max_y) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  })
  
  # 3. Assemble and Save — matches original apshape_full layout
  clustervis   <- gridExtra::grid.arrange(grobs = cluster_plots, ncol = 4)
  apshape_full <- gridExtra::grid.arrange(main_ap_vis, clustervis, ncol = 2)
  ggplot2::ggsave("ap_plot.png", apshape_full, path = save_path, width = 15, height = 15)
  
  return(cluster_plots)
}

# analyze_firing_probabilities---------------------------------------------------------
# Firing & Multi-fire Analyzer
# Fn: Calculates cluster engagement stats and multi-firing distribution
# Exec: prob_results <- analyze_firing_probabilities(cluster_list, rri_sheet, burst_sheet, plots_folder)
# Returns: list(data, prob_plots, beats_prob_list, multi_plots)
# ---------------------------------------------------------
analyze_firing_probabilities <- function(cluster_list, rri_sheet, burst_sheet, save_path) {
  
  # 1. Setup Baselines
  beats_total <- nrow(rri_sheet)
  burst_total <- max(burst_sheet$`Burst Number`, na.rm = TRUE)
  
  cluster_prob_list <- list()   # per-burst probability plots  (original: cluster_prob_list)
  beats_prob_list   <- list()   # per-beat  probability plots  (original: beats_prob_list)
  multifire_list    <- list()
  stat_summary      <- list()
  
  # 2. Process each cluster
  for (i in seq_along(cluster_list)) {
    cluster     <- as.data.frame(cluster_list[[i]])
    cluster     <- cluster[-c(12:44)]
    names(cluster)[8] <- "ap_loc_sec"
    cluster_num <- levels(cluster$`Cluster Number`)[i]
    
    # --- Firing Probabilities ---
    unique_bursts  <- length(unique(cluster$`Burst Number`))
    beats_prob     <- unique_bursts / beats_total * 100
    offbeats_prob  <- (beats_total - unique_bursts) / beats_total * 100
    cluster_prob   <- unique_bursts / burst_total * 100
    off_prob       <- (burst_total - unique_bursts) / burst_total * 100
    
    beats_df      <- cbind.data.frame(offbeats_prob, beats_prob)
    beats_df_melt <- reshape2::melt(beats_df, id.vars = character(0))
    prob_df       <- cbind.data.frame(off_prob, cluster_prob)
    prob_df_melt  <- reshape2::melt(prob_df,  id.vars = character(0))
    
    # Burst probability plot (original: cluster_prob_list)
    cluster_prob_list[[i]] <- ggplot2::ggplot(prob_df_melt, ggplot2::aes(variable, value, fill = variable)) +
      ggplot2::geom_col() +
      ggplot2::ggtitle(paste("cluster", cluster_num)) +
      ggplot2::ylim(0, 100) +
      ggplot2::ylab("Probability") +
      ggplot2::xlab("") +
      ggsci::scale_fill_jco() +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_classic()
    
    # Beat probability plot (original: beats_prob_list)
    beats_prob_list[[i]] <- ggplot2::ggplot(beats_df_melt, ggplot2::aes(variable, value, fill = variable)) +
      ggplot2::geom_col() +
      ggplot2::ggtitle(paste("cluster", cluster_num)) +
      ggplot2::ylim(0, 100) +
      ggplot2::ylab("Probability") +
      ggplot2::xlab("") +
      ggsci::scale_fill_jco() +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_classic()
    
    # --- Multi-firing Logic ---
    cluster_counts <- table(cluster$`Burst Number`)
    summary_table  <- table(factor(cluster_counts, levels = 1:6, labels = c("1","2","3","4","5","6+")))
    multifire_df   <- as.data.frame(summary_table)
    colnames(multifire_df) <- c("Multiple_firings", "Frequency")
    freq_total     <- sum(multifire_df$Frequency)
    multifire_df$Probability <- multifire_df$Frequency / freq_total * 100
    
    multifire_list[[i]] <- ggplot2::ggplot(multifire_df, ggplot2::aes(Multiple_firings, Probability, fill = Multiple_firings)) +
      ggplot2::geom_col() +
      ggplot2::ggtitle(paste("cluster", cluster_num)) +
      ggplot2::ylim(0, 100) +
      ggsci::scale_fill_jco() +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_classic()
    
    # --- Store Statistics for CSV ---
    stat_summary[[i]] <- data.frame(
      cluster     = cluster_num,
      beats_prob  = beats_prob,
      bursts_prob = cluster_prob,
      t(setNames(multifire_df$Frequency,    paste0("freq_",  multifire_df$Multiple_firings))),
      t(setNames(multifire_df$Probability,  paste0("prob_",  multifire_df$Multiple_firings)))
    )
  }
  
  # 3. Assemble and Save Grids
  cluster_prob_grid <- gridExtra::arrangeGrob(
    grobs = cluster_prob_list, ncol = 4,
    top   = grid::textGrob("clusters", gp = grid::gpar(fontsize = 16, fontface = "bold"))
  )
  multifire_grid <- gridExtra::grid.arrange(grobs = multifire_list, ncol = 4)
  
  ggplot2::ggsave("cluster_prob.png",   cluster_prob_grid, path = save_path, width = 15, height = 15)
  ggplot2::ggsave("multifire_prob.png", multifire_grid,    path = save_path, width = 15, height = 15)
  
  # 4. Build combined prob_data CSV exactly as original (beats + bursts + multifire merged)
  beats_data <- beats_prob_list %>%
    imap_dfr(~ {
      .x$data %>%
        mutate(cluster = .y) %>%
        select(cluster, variable, value)
    }) %>%
    pivot_wider(names_from = variable, values_from = value, names_glue = "{variable}_prob") %>%
    rename_with(~ gsub("_prob_prob", "_prob", .x))
  
  bursts_data <- cluster_prob_list %>%
    imap_dfr(~ {
      .x$data %>%
        mutate(cluster = .y) %>%
        select(cluster, variable, value)
    }) %>%
    pivot_wider(names_from = variable, values_from = value, names_glue = "{variable}_prob") %>%
    rename_with(~ gsub("_prob_prob", "_prob", .x))
  
  prob_data <- merge(beats_data, bursts_data, by = "cluster")
  
  multifire_data <- multifire_list %>%
    imap_dfr(~ {
      .x$data %>%
        mutate(cluster = .y) %>%
        select(cluster, Multiple_firings, Frequency, Probability)
    }) %>%
    pivot_wider(
      names_from  = Multiple_firings,
      values_from = c(Frequency, Probability),
      names_glue  = "{.value}_{Multiple_firings}"
    )
  
  prob_data <- merge(prob_data, multifire_data, by = "cluster")
  
  # Save base probabilities CSV (original saves this as _probabilities.csv)
  
  # 5. Return all four lists so downstream functions can use them
  return(list(
    data             = do.call(rbind, stat_summary),
    prob_plots       = cluster_prob_list,   # burst-level probability plots
    beats_prob_list  = beats_prob_list,     # beat-level probability plots
    multi_plots      = multifire_list,
    prob_data        = prob_data
  ))
}

# dbscan_isolate_neuron_data---------------------------------------------------------
# Neuron Isolation Processor
# Fn: Sub-clusters within shape groups and cleans neuron IDs
# Exec: neurons_df <- isolate_neuron_data(cluster_list, eps=0.7, minPts=4)
# ---------------------------------------------------------
dbscan_isolate_neuron_data <- function(cluster_list, eps = 0.7, minPts = 4) {
  
  processed_list <- list()
  
  for (i in seq_along(cluster_list)) {
    raw_df  <- as.data.frame(cluster_list[[i]])
    # Select by position then rename — matches original lat_amp[c(1,3,5,9)] + colnames()
    # Cols: 1=Burst Number, 3=ap_id, 5=AP Amplitude, 9=AP Latency (exact positions post-merge)
    lat_amp <- raw_df[, c(1, 3, 5, 9)]
    colnames(lat_amp) <- c("burst_number", "ap_id", "ap_amp", "ap_latency")
    
    scaled_lat           <- scale(as.matrix(lat_amp$ap_latency))
    scaled_lat[is.nan(scaled_lat)] <- 0
    db_res               <- dbscan::dbscan(scaled_lat, eps = eps, minPts = minPts)
    
    n_id        <- db_res$cluster
    n_id[n_id == 0] <- 1   # Recode noise (0) to cluster 1, matching original
    lat_amp$neuron_id <- as.factor(n_id)
    
    processed_list[[i]] <- merge(raw_df, lat_amp[, c("ap_id", "neuron_id")], by = "ap_id")
  }
  
  full_neurons_df <- do.call(rbind, processed_list)

  # Rename by position — col 3=ap_id already named, col 5=AP Amplitude, col 8=AP Location, col 9=AP Latency
  # These match the original ap_sheet column order used throughout the script
  names(full_neurons_df)[5] <- "ap_amp"
  names(full_neurons_df)[8] <- "ap_loc_sec"
  names(full_neurons_df)[9] <- "ap_latency"

  full_neurons_df$neuron_cluster_id <- paste(
    full_neurons_df$`Cluster Number`,
    as.character(full_neurons_df$neuron_id),
    sep = "_"
  )
  
  return(full_neurons_df)
}

# sort_clusters_by_amplitude---------------------------------------------------------
# Burst Intensity Sorter
# Fn: Merges RRI and Burst metrics, then sorts data by Burst Amplitude
# Exec: sorted_clusters <- sort_clusters_by_amplitude(neurons_df, rri_sheet, burst_sheet)
# ---------------------------------------------------------
sort_clusters_by_amplitude <- function(neurons_df, rri_sheet, burst_sheet) {
  
  # 1. Clean and Prepare RRI data
  burst_rri <- rri_sheet[, c(1, 6)]
  colnames(burst_rri) <- c("Burst Number", "RRI")
  burst_rri <- dplyr::filter(burst_rri, `Burst Number` != 0)
  
  # 2. Merge RRI with the Neuron Data
  enriched_df <- merge(neurons_df, burst_rri, by = "Burst Number")
  
  # 3. Remove shape vectors (cols 12:43)
  enriched_df <- enriched_df[, -c(12:43)]
  
  # 4. Attach Burst Amplitude for sorting
  sort_metrics <- burst_sheet[, c("Burst Number", "Burst Amp")]
  enriched_df  <- merge(enriched_df, sort_metrics, by = "Burst Number")
  
  # 5. Sort by Cluster Number then Burst Amplitude
  enriched_df <- enriched_df %>%
    dplyr::arrange(`Cluster Number`, `Burst Amp`)
  
  # 6. Split into list by Cluster
  sorted_cluster_list <- split(enriched_df, enriched_df$`Cluster Number`)
  
  return(sorted_cluster_list)
}

# summarize_ap_metrics---------------------------------------------------------
# Latency & Amplitude Summarizer
# Fn: Calculates burst-level means and cluster-level descriptive stats
# Exec: ap_metrics <- summarize_ap_metrics(neurons_df, file_id_folder, file_id)
# ---------------------------------------------------------
summarize_ap_metrics <- function(neurons_df, folder_path, file_id) {
  
  # 1. Detailed Locator Data (Burst x Cluster x Neuron) — matches original LOCATOR_AP.csv
  locator_ap_latency <- neurons_df %>%
    dplyr::group_by(`Burst Number`, `Cluster Number`, neuron_id) %>%
    dplyr::summarise(
      locator_ap_latency = mean(ap_latency, na.rm = TRUE),
      sd_ap_latency      = sd(ap_latency,   na.rm = TRUE),
      .groups = "drop"
    )
  

  
  # 2. Population Summary (Median/IQR per Cluster) — matches original _apd_latency_summary.csv
  mean_ap_latency_summary <- neurons_df %>%
    dplyr::group_by(`Cluster Number`) %>%
    dplyr::summarise(across(
      c(ap_latency, ap_amp),
      list(
        median = ~median(., na.rm = TRUE),
        iqr    = ~IQR(.,    na.rm = TRUE),
        q1     = ~quantile(., 0.25, na.rm = TRUE),
        q3     = ~quantile(., 0.75, na.rm = TRUE)
      )
    ))
  

  
  return(list(locator = locator_ap_latency, summary = mean_ap_latency_summary))
}

# split_list_by_neuron---------------------------------------------------------
# Neuron Sub-Splitter
# Fn: Splits cluster list into individual neurons (e.g., "1" and "1_2")
# Exec: neuron_list <- split_list_by_neuron(dbscan_split_clusters_list)
# ---------------------------------------------------------
split_list_by_neuron <- function(original_list) {
  result_list <- list()
  
  purrr::walk(seq_along(original_list), function(i) {
    item <- original_list[[i]]
    
    if ("neuron_id" %in% names(item) && is.factor(item$neuron_id)) {
      level1 <- item %>% dplyr::filter(neuron_id != 2)
      result_list[[as.character(i)]] <<- level1
      
      level2 <- item %>% dplyr::filter(neuron_id == 2)
      if (nrow(level2) > 0) {
        result_list[[paste0(i, "_2")]] <<- level2
      }
    } else {
      result_list[[as.character(i)]] <<- item
    }
  })
  return(result_list)
}

# analyze_neuron_probabilities---------------------------------------------------------
# DBSCAN Neuron Analyzer
# Fn: Calculates probabilities and multi-fire stats for individual neurons
# Exec: db_stats <- analyze_neuron_probabilities(neuron_list, beats_total, burst_total, file_id_folder, file_id)
# ---------------------------------------------------------
analyze_neuron_probabilities <- function(neuron_list, beats_total, burst_total, folder_path, file_id) {
  
  dbcluster_prob_list <- vector("list", length(neuron_list))
  names(dbcluster_prob_list) <- names(neuron_list)
  dbbeats_prob_list <- vector("list", length(neuron_list))
  names(dbbeats_prob_list) <- names(neuron_list)
  dbmultifire_list <- vector("list", length(neuron_list))
  names(dbmultifire_list) <- names(neuron_list)
  
  for (i in seq_along(neuron_list)) {
    cluster      <- as.data.frame(neuron_list[[i]])
    if (ncol(cluster) >= 44) cluster <- cluster[-c(12:44)]
    names(cluster)[8] <- "ap_loc_sec"
    original_name <- names(neuron_list)[i]
    
    # Probabilities
    unique_bursts <- length(unique(cluster$`Burst Number`))
    beats_prob    <- unique_bursts / beats_total * 100
    offbeats_prob <- (beats_total - unique_bursts) / beats_total * 100
    cluster_prob  <- unique_bursts / burst_total  * 100
    off_prob      <- (burst_total  - unique_bursts) / burst_total * 100
    
    beats_df      <- cbind.data.frame(offbeats_prob, beats_prob)
    beats_df_melt <- reshape2::melt(beats_df, id.vars = character(0))
    prob_df       <- cbind.data.frame(off_prob, cluster_prob)
    prob_df_melt  <- reshape2::melt(prob_df,  id.vars = character(0))
    
    dbcluster_prob_list[[i]] <- ggplot2::ggplot(prob_df_melt, ggplot2::aes(variable, value, fill = variable)) +
      ggplot2::geom_col() +
      ggplot2::ggtitle(paste("cluster", original_name)) +
      ggplot2::ylim(0, 100) +
      ggplot2::ylab("Probability") + ggplot2::xlab("") +
      ggsci::scale_fill_jco() + ggplot2::guides(fill = "none") +
      ggplot2::theme_classic()
    
    dbbeats_prob_list[[i]] <- ggplot2::ggplot(beats_df_melt, ggplot2::aes(variable, value, fill = variable)) +
      ggplot2::geom_col() +
      ggplot2::ggtitle(paste("cluster", original_name)) +
      ggplot2::ylim(0, 100) +
      ggplot2::ylab("Probability") + ggplot2::xlab("") +
      ggsci::scale_fill_jco() + ggplot2::guides(fill = "none") +
      ggplot2::theme_classic()
    
    # Multi-fire
    cluster_counts <- table(cluster$`Burst Number`)
    summary_table  <- table(factor(cluster_counts, levels = 1:6, labels = c("1","2","3","4","5","6+")))
    dbmultifire_df <- as.data.frame(summary_table)
    colnames(dbmultifire_df) <- c("Multiple_firings", "Frequency")
    freq_total     <- sum(dbmultifire_df$Frequency)
    dbmultifire_df$Probability <- dbmultifire_df$Frequency / freq_total * 100
    
    dbmultifire_list[[i]] <- ggplot2::ggplot(dbmultifire_df, ggplot2::aes(Multiple_firings, Probability, fill = Multiple_firings)) +
      ggplot2::geom_col() +
      ggplot2::ggtitle(paste("cluster", original_name)) +
      ggplot2::ylim(0, 100) +
      ggsci::scale_fill_jco() + ggplot2::guides(fill = "none") +
      ggplot2::theme_classic()
  }
  
  # Reshape — exactly as original
  dbbeats_data <- dbbeats_prob_list %>%
    imap_dfr(~ { .x$data %>% mutate(cluster = .y) %>% select(cluster, variable, value) }) %>%
    pivot_wider(names_from = variable, values_from = value, names_glue = "{variable}_prob") %>%
    rename_with(~ gsub("_prob_prob", "_prob", .x))
  
  dbbursts_data <- dbcluster_prob_list %>%
    imap_dfr(~ { .x$data %>% mutate(cluster = .y) %>% select(cluster, variable, value) }) %>%
    pivot_wider(names_from = variable, values_from = value, names_glue = "{variable}_prob") %>%
    rename_with(~ gsub("_prob_prob", "_prob", .x))
  
  dbprob_data <- merge(dbbeats_data, dbbursts_data, by = "cluster")
  
  dbmultifire_data <- dbmultifire_list %>%
    imap_dfr(~ {
      .x$data %>%
        mutate(cluster = .y) %>%
        select(cluster, Multiple_firings, Frequency, Probability)
    }) %>%
    pivot_wider(
      names_from  = Multiple_firings,
      values_from = c(Frequency, Probability),
      names_glue  = "{.value}_{Multiple_firings}"
    )
  
  dbprob_data <- merge(dbprob_data, dbmultifire_data, by = "cluster")

  
  # Latency summary (DBSCAN-level)
  df_dbmean <- bind_rows(neuron_list, .id = "cluster_name")
  dbmean_ap_latency <- df_dbmean %>%
    dplyr::group_by(cluster_name) %>%
    dplyr::summarise(across(
      c(ap_latency, ap_amp),
      list(
        median = ~median(., na.rm = TRUE),
        iqr    = ~IQR(.,    na.rm = TRUE),
        q1     = ~quantile(., 0.25, na.rm = TRUE),
        q3     = ~quantile(., 0.75, na.rm = TRUE)
      )
    ))

  
  return(list(
    dbprob_data       = dbprob_data,
    dbmean_ap_latency = dbmean_ap_latency,
    dbcluster_prob_list = dbcluster_prob_list,
    dbbeats_prob_list   = dbbeats_prob_list,
    dbmultifire_list    = dbmultifire_list
  ))
}

# analyze_isi---------------------------------------------------------
# ISI Analyzer 
# Fn: Calculates within-burst inter-spike intervals using absolute ap_loc_sec
# Exec: isi_raw  <- analyze_isi(neurons_df,             file_id_folder, file_id)
#       isi_norm <- analyze_isi(norm_bundle$normalized, file_id_folder, file_id, suffix = "_NORM")
# ---------------------------------------------------------
analyze_isi <- function(neurons_df, folder_path, file_id, suffix = "", split_by = "Cluster Number") {

  # ISI calculator — gap between consecutive APs within each burst+cluster
  calculate_diff_info <- function(df) {
    df %>%
      dplyr::group_by(`Burst Number`, `Cluster Number`) %>%
      dplyr::arrange(ap_loc_sec) %>%
      dplyr::mutate(ap_isi = ap_loc_sec - dplyr::lag(ap_loc_sec)) %>%
      dplyr::filter(!is.na(ap_isi)) %>%
      dplyr::select(`Burst Number`, `Cluster Number`, ap_isi)
  }

  split_clusters  <- split(neurons_df, neurons_df[[split_by]])
  all_differences <- dplyr::bind_rows(lapply(split_clusters, calculate_diff_info), .id = "Cluster")

  summary_list <- list()

  for (cluster in unique(all_differences$Cluster)) {
    cluster_data <- all_differences %>% dplyr::filter(Cluster == cluster)

    multicount <- cluster_data %>%
      dplyr::group_by(Cluster, `Burst Number`) %>%
      dplyr::mutate(pair_count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        n_2fire    = as.integer(pair_count == 1),
        n_3fire    = as.integer(pair_count == 2),
        n_4fire    = as.integer(pair_count == 3),
        `n_5+fire` = as.integer(pair_count >= 4)
      )
firing_stats <- multicount %>%
      dplyr::mutate(fire_group = dplyr::case_when(
        n_2fire    == 1 ~ "2fire",
        n_3fire    == 1 ~ "3fire",
        n_4fire    == 1 ~ "4fire",
        `n_5+fire` == 1 ~ "5fire"
      )) %>%
      dplyr::filter(!is.na(fire_group)) %>%
      dplyr::group_by(fire_group) %>%
      dplyr::summarise(
        me  = mean(ap_isi,   na.rm = TRUE),
        sd  = sd(ap_isi,     na.rm = TRUE),
        md  = median(ap_isi, na.rm = TRUE),
        iqr = IQR(ap_isi,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from  = fire_group,
        values_from = c(me, sd, md, iqr),
        names_glue  = "{fire_group}_{.value}"
      )

    summary_list[[cluster]] <- dplyr::bind_cols(
      data.frame(
        file_id,
        cluster,
        cl_isi_count  = nrow(cluster_data),
        cl_ap_isi_me  = mean(cluster_data$ap_isi,   na.rm = TRUE),
        cl_ap_isi_sd  = sd(cluster_data$ap_isi,     na.rm = TRUE),
        cl_ap_isi_md  = median(cluster_data$ap_isi, na.rm = TRUE),
        cl_ap_isi_iqr = IQR(cluster_data$ap_isi,    na.rm = TRUE),
        cl_ap_isi_min = min(cluster_data$ap_isi,    na.rm = TRUE),
        cl_ap_isi_max = max(cluster_data$ap_isi,    na.rm = TRUE),
        n_2fire       = sum(multicount$n_2fire),
        n_3fire       = sum(multicount$n_3fire),
        n_4fire       = sum(multicount$n_4fire),
        n_5fire       = sum(multicount$`n_5+fire`),
        stringsAsFactors = FALSE
      ),
      firing_stats
    )
  }
  output_df <- dplyr::bind_rows(summary_list)
  return(list(data = output_df, all_differences = all_differences))
}
# plot_isi---------------------------------------------------------
# ISI Figure Generator
# Fn: Density + jitter plot per cluster from analyze_isi output
# Exec: plot_isi(isi_raw, plots_folder, file_name)
# ---------------------------------------------------------
plot_isi <- function(isi_result, plots_folder, file_name) {
  
  all_differences <- isi_result$all_differences
  max_isi         <- max(all_differences$ap_isi, na.rm = TRUE)
  plots_list      <- list()
  
  for (cluster in unique(all_differences$Cluster)) {
    cluster_data  <- all_differences %>% dplyr::filter(Cluster == cluster)
    cl_ap_isi_md  <- median(cluster_data$ap_isi, na.rm = TRUE)
    cl_ap_isi_me  <- mean(cluster_data$ap_isi,   na.rm = TRUE)
    
    plots_list[[cluster]] <- ggplot2::ggplot(
      cluster_data, ggplot2::aes(x = ap_isi, fill = as.factor(`Burst Number`), color = as.factor(`Burst Number`))
    ) +
      ggplot2::geom_density(
        data = cluster_data, ggplot2::aes(x = ap_isi),
        color = "black", fill = "grey", alpha = 0.3, linewidth = 1
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0, height = 0.1),
        ggplot2::aes(y = 0), size = 2
      ) +
      ggplot2::geom_vline(xintercept = cl_ap_isi_md, linetype = "dashed", color = "red") +
      ggplot2::geom_vline(xintercept = cl_ap_isi_me, linetype = "solid",  color = "blue") +
      ggplot2::xlim(0, max_isi) +
      ggplot2::labs(title = paste("Cluster", cluster)) +
      ggpubr::theme_classic2() +
      ggplot2::annotate("text", x = cl_ap_isi_md, y = 3, label = paste("median", round(cl_ap_isi_md, 2))) +
      ggplot2::annotate("text", x = cl_ap_isi_me, y = 1, label = paste("mean",   round(cl_ap_isi_me, 2))) +
      ggplot2::theme(
        axis.title.x  = ggplot2::element_blank(),
        axis.title.y  = ggplot2::element_blank(),
        axis.text.y   = ggplot2::element_blank(),
        legend.position = "none"
      )
  }
  
  file_label    <- grid::textGrob(paste("File:", file_name), gp = grid::gpar(fontsize = 10))
  combined_plot <- do.call(gridExtra::grid.arrange, c(plots_list, list(file_label), ncol = 1))
  
  ggplot2::ggsave(
    file.path(plots_folder, paste0(file_name, "_isi_plot.png")),
    plot   = combined_plot,
    width  = 6,
    height = length(plots_list) * 1.5
  )
  
  return(combined_plot)
}
# create_master_summary---------------------------------------------------------
# Master Cluster Summary Generator
# Fn: Builds cluster_summary.png and latency_amplitude.png; saves all final CSVs
# Exec: create_master_summary(cluster_list, neurons_df, shape_plots, beats_prob_list,
#                             cluster_prob_list, multifire_list, ap_sheet,
#                             total_time, plots_folder, file_id_folder, file.id,
#                             clusters_sheet, dbprob_data, dbmean_ap_latency)
# ---------------------------------------------------------
create_master_summary <- function(cluster_list, neurons_df, shape_plots,beats_prob_list, cluster_prob_list, multifire_list,ap_sheet, total_time,plots_folder, file_id_folder, file_id,clusters_sheet, dbprob_data, dbmean_ap_latency) {
  
  # --- 1. DBSCAN Lat/Amp plots (one per base cluster) ---
  # Use full ap_sheet range for xlim — matches original exactly
  min_latency <- min(ap_sheet$`AP Latency (s)`, na.rm = TRUE)
  max_latency <- max(ap_sheet$`AP Latency (s)`, na.rm = TRUE)
  
  # Colour scheme matching original: "0"=grey (noise), "1"=blue, "2"=yellow, "3"=red
  cols <- c("0" = "#868686FF", "1" = "#0073C2FF", "2" = "#EFC000FF", "3" = "#CD534CFF")
  
  cluster_lat_amp_list <- list()
  lat_amp_with_clusters_list <- list()
  
  for (i in seq_along(cluster_list)) {
    lat_amp <- as.data.frame(cluster_list[[i]])
    lat_amp <- lat_amp[c(1, 3, 5, 9)]  # burst_number, ap_id, ap_amp, ap_latency — same cols as original
    colnames(lat_amp) <- c("burst_number", "ap_id", "ap_amp", "ap_latency")
    
    scaled_data              <- scale(as.matrix(lat_amp$ap_latency))
    scaled_data[is.nan(scaled_data)] <- 0
    dbscan_result            <- dbscan::dbscan(scaled_data, eps = 0.7, minPts = 4)
    cluster_assignments      <- dbscan_result$cluster
    
    lat_amp_with_clusters    <- cbind(lat_amp, Cluster = factor(cluster_assignments))
    names(lat_amp_with_clusters)[5] <- "neuron_id"
    lat_amp_with_clusters_list[[i]] <- lat_amp_with_clusters
    
    cluster_lat_amp_list[[i]] <- ggplot2::ggplot(
      lat_amp_with_clusters, ggplot2::aes(x = ap_latency, y = ap_amp, color = neuron_id)
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_density(
        inherit.aes = FALSE,
        data    = lat_amp_with_clusters,
        ggplot2::aes(ap_latency),
        adjust  = 0.5,
        alpha   = 0.5
      ) +
      ggplot2::labs(x = "Latency", y = "Amplitude", color = "DBSCAN") +
      ggplot2::xlim(min_latency, max_latency) +
      ggplot2::ggtitle(paste("cluster", levels(cluster_list[[i]]$`Cluster Number`)[i])) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::theme_classic()
  }
  
  # --- 2. latency_amplitude.png (standalone, as original) ---
  lat_amp_summary <- gridExtra::grid.arrange(grobs = cluster_lat_amp_list, ncol = 1)
  gridheight      <- length(cluster_lat_amp_list) * 2
  lat_amp_summary <- gridExtra::arrangeGrob(lat_amp_summary, top = file_name)
  ggplot2::ggsave("latency_amplitude.png", lat_amp_summary,
                  path = plots_folder, height = gridheight, width = 15, limitsize = FALSE)
  
  # --- 3. all_lat_amp_plot.png (standalone, as original) ---
  all_lat_amp <- ap_sheet[c(4, 8, 9)]
  all_lat_amp <- tidyr::drop_na(all_lat_amp)
  colnames(all_lat_amp) <- c("ap_amp", "ap_latency", "Cluster")
  all_lat_amp$Cluster   <- as.integer(all_lat_amp$Cluster)
  
  all_lat_amp_plot <- ggplot2::ggplot(all_lat_amp, ggplot2::aes(ap_latency, ap_amp, color = Cluster)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Latency", y = "Amplitude", color = "Cluster") +
    ggplot2::xlim(min_latency, max_latency) +
    ggplot2::ggtitle("Amplitude/Latency") +
    ggplot2::scale_color_continuous(type = "viridis") +
    ggplot2::theme_classic()
  ggplot2::ggsave("all_lat_amp_plot.png", all_lat_amp_plot,
                  path = plots_folder, height = 15, width = 15)
  
  # --- 4. cluster_summary.png ---
  # Layout: shape | beats_prob | cluster_prob | multifire | lat_amp  (5 cols, widths c(1,1,1,2,2))
  # Exactly matches original combined_grobs_list loop
  combined_grobs_list <- list()
  for (i in seq_along(shape_plots)) {
    combined_plot <- gridExtra::grid.arrange(
      shape_plots[[i]],
      beats_prob_list[[i]],
      cluster_prob_list[[i]],
      multifire_list[[i]],
      cluster_lat_amp_list[[i]],
      widths = c(1, 1, 1, 2, 2),
      ncol   = 5
    )
    combined_grobs_list <- c(combined_grobs_list, list(combined_plot))
  }
  
  grid         <- gridExtra::grid.arrange(grobs = combined_grobs_list, ncol = 1)
  cluster_summary <- gridExtra::arrangeGrob(grid, top = file_name)
  gridheight   <- length(combined_grobs_list) * 2
  ggplot2::ggsave("cluster_summary.png", cluster_summary,
                  path = plots_folder, height = gridheight, width = 15, limitsize = FALSE)
  
  # --- 5. Reference Binning CSV ---
  extract_numbers <- function(text) {
    numbers <- stringr::str_extract_all(text, "\\d+\\.\\d+")[[1]]
    return(as.numeric(numbers))
  }
  n           <- ncol(clusters_sheet)
  normal_min  <- extract_numbers(clusters_sheet[5, 1])[1]
  normal_max  <- extract_numbers(clusters_sheet[5, n])[2]
  
  clusters_sheet_row2 <- c(clusters_sheet[2, ])
  clusters_lat_sheet  <- c(clusters_sheet[3, ])
  clusters_desc <- cbind.data.frame(
    cluster_amplitude = as.numeric(clusters_sheet_row2),
    cluster_latency   = as.numeric(clusters_lat_sheet)
  )
  
  normalize_clusters <- function(clusters_desc, normal, normal_min) {
    if (nrow(clusters_desc) < 10) {
      warning("Fewer than 10 clusters - some bins may be empty or contain few points")
    }
    percentile_breaks <- seq(normal_min, normal, length.out = 11)
    clusters_desc$percentile_bin <- cut(
      clusters_desc$cluster_amplitude,
      breaks          = percentile_breaks,
      include.lowest  = TRUE,
      labels          = FALSE
    )
    result <- aggregate(cbind(cluster_amplitude, cluster_latency) ~ percentile_bin,
                        data = clusters_desc, FUN = median)
    result$percentile_range <- paste0((result$percentile_bin - 1) * 10, "-", result$percentile_bin * 10, "%")
    result <- result[order(result$percentile_bin),
                     c("percentile_bin", "percentile_range", "cluster_amplitude", "cluster_latency")]
    return(result)
  }
  
  binned_clusters <- normalize_clusters(clusters_desc, normal_max, normal_min)

  return(list(grid = cluster_summary, binned = binned_clusters))
}

# export_all_results---------------------------------------------------------
# Master CSV Exporter
# Fn: Single point of truth for all write.csv output — called once per file at end of pipeline
# Exec: export_all_results(folder_path, norm_bundle, ap_metrics, base_prob_results,
#                          db_stats_bundle, isi_raw, isi_norm, master_summary)
# ---------------------------------------------------------
export_all_results <- function(folder_path, norm_bundle, ap_metrics, base_prob_results, db_stats_bundle, isi_raw, isi_norm, master_summary) {
  
  # Helper to write with consistent row.names = FALSE
save_csv <- function(data, filename) {
    write.csv(data, file.path(folder_path, paste0(file_name, filename)), row.names = FALSE)
  }
  
  # --- Neuron data (raw and normalised) ---
  save_csv(norm_bundle$raw,        "_RAW_neurons.csv")
  save_csv(norm_bundle$normalized, "_NORM_PERCENT_neurons.csv")
  
  # --- AP latency / amplitude summaries ---
  save_csv(ap_metrics$locator, "_LOCATOR_AP.csv")
  save_csv(ap_metrics$summary, "_apd_latency_summary.csv")
  
  # --- Base cluster firing probabilities ---
  save_csv(base_prob_results$prob_data, "_probabilities.csv")
  
  # --- DBSCAN neuron-level probabilities and latency ---
  save_csv(db_stats_bundle$dbprob_data,       "_DBSCAN_probabilities.csv")
  save_csv(db_stats_bundle$dbmean_ap_latency, "_DBSCAN_apd_latency_summary.csv")
  
  # --- ISI summaries (raw and normalised) ---
  save_csv(isi_raw$data,  "_ISI_summary.csv")
  save_csv(isi_norm$data, "_ISI_summary_NORM.csv")
  
  # --- Normalised cluster description (percentile bins) ---
  save_csv(master_summary$binned, "_NORMALIZED_cluster_description.csv")
  
  message("  Exports saved for: ", file_name)
}

# archive_analysis_session---------------------------------------------------------
# Session Archiver
# Fn: Saves full workspace and environment RDS for the current file
# Exec: archive_analysis_session(file_id_folder, file_id)
# ---------------------------------------------------------
archive_analysis_session <- function(folder_path, file_id) {
  save.image(file.path(folder_path, paste0(file_id, "_session.RData")))
  saveRDS(environment(), file.path(folder_path, paste0(file_id, "_environment.RDS")))
}

#------------------------------------------MAINLOOP------------------------------------------------#
print("Starting DBSCAN Analysis...")
GITHUB_RAW_URL  <- "https://github.com/nkcheung95/MSNA-APD-Post-processing/blob/main/dbscan_script.R"
current_version <- get_github_version(GITHUB_RAW_URL)

for (file_id in names(all_data)) {
  file_name <- substr(file_id, 1, nchar(file_id) - 10)
  message("--- Analyzing: ", file_name, " ---")

  if (is_already_analyzed(analyzed_folder, file_id, file_name, current_version)) next

  tryCatch({
  # 1. Extract Sheets for Current File
  summ_sheet      <- all_data[[file_id]]$summ
  burst_sheet     <- all_data[[file_id]]$burst
  ap_sheet        <- all_data[[file_id]]$ap
  ap_shapes_sheet <- all_data[[file_id]]$ap_shapes
  clusters_sheet  <- all_data[[file_id]]$clusters
  rri_sheet       <- all_data[[file_id]]$rri
  
  # 2. Setup Folders
  file_id_folder <- file.path(analyzed_folder, file_id)
  if (!dir.exists(file_id_folder)) dir.create(file_id_folder, recursive = TRUE)
  plots_folder <- file.path(file_id_folder, "plots")
  if (!dir.exists(plots_folder)) dir.create(plots_folder)
  

  # 3. Setup Baselines
  beats_total <- nrow(rri_sheet)
  burst_total <- max(burst_sheet$`Burst Number`, na.rm = TRUE)
  total_time  <- as.numeric(max(summ_sheet$`Data Duration (s)`, na.rm = TRUE))
  
  # ---------------------------------------------------------
  # CORE_PROCESSING_PIPELINE----------------------------------
  #
  # Step A: Data Prep & Base Visuals
  cluster_list <- prep_apd_data(ap_sheet, ap_shapes_sheet)
  main_ap_vis  <- generate_smoothed_ap(ap_shapes_sheet)
  
  # Step B: Shape Plotting & Base Probabilities
  shape_plots       <- visualize_apd_clusters(cluster_list, main_ap_vis, clusters_sheet, plots_folder)
  base_prob_results <- analyze_firing_probabilities(cluster_list, rri_sheet, burst_sheet, plots_folder)
  
  # Step C: DBSCAN Isolation & Metric Summaries
  neurons_df <- dbscan_isolate_neuron_data(cluster_list, eps = 0.7, minPts = 1)
  ap_metrics <- summarize_ap_metrics(neurons_df, file_id_folder, file_name)
  
  # Step D: Normalize neurons
  norm_bundle <- normalize_neurons_full(neurons_df, clusters_sheet, file_id_folder, file_name)
  
  # Step E: ISI Analysis (master timescale) — raw and normalized with plots
  isi_raw  <- analyze_isi(neurons_df,             file_id_folder, file_name)
  isi_norm <- analyze_isi(norm_bundle$normalized, file_id_folder, file_name, suffix = "_NORM", split_by = "amp_percentile_bin")
  plot_isi(isi_raw,  plots_folder, file_name)
  plot_isi(isi_norm, plots_folder, paste0(file_name, "_NORM"))

  # Step F: Split by Sub-Neuron & Analyze Final DBSCAN Probabilities
  dbscan_cluster_list <- split(neurons_df, neurons_df$`Cluster Number`)
  neuron_list         <- split_list_by_neuron(dbscan_cluster_list)
  db_stats_bundle     <- analyze_neuron_probabilities(
    neuron_list, beats_total, burst_total, file_id_folder, file_name
  )
  
  # Step G: Master Summary Grid (plots only)
  master_summary <- create_master_summary(
    cluster_list      = cluster_list,
    neurons_df        = neurons_df,
    shape_plots       = shape_plots,
    beats_prob_list   = base_prob_results$beats_prob_list,
    cluster_prob_list = base_prob_results$prob_plots,
    multifire_list    = base_prob_results$multi_plots,
    ap_sheet          = ap_sheet,
    total_time        = total_time,
    plots_folder      = plots_folder,
    file_id_folder    = file_id_folder,
    file_id           = file_name,
    clusters_sheet    = clusters_sheet,
    dbprob_data       = db_stats_bundle$dbprob_data,
    dbmean_ap_latency = ap_metrics$summary
  )
  
  # Step H: Export all CSVs — single point of truth for all output files
  export_all_results(
    folder_path       = file_id_folder,
    norm_bundle       = norm_bundle,
    ap_metrics        = ap_metrics,
    base_prob_results = base_prob_results,
    db_stats_bundle   = db_stats_bundle,
    isi_raw           = isi_raw,
    isi_norm          = isi_norm,
    master_summary    = master_summary
  )
  
  # Step I: Archive Session Data
  archive_analysis_session(file_id_folder, file_name)
  
    # Write version stamp on successful completion
    writeLines(current_version, file.path(file_id_folder, "analysis_version.txt"))

    message("Done: ", file_name)

  }, error = function(e) {
    message("ERROR in ", file_id, ": ", conditionMessage(e))
    failed_files <<- c(failed_files, file_id)
  })

  # Clean environment for next loop iteration (keeps all functions and shared data intact)
  rm(list = setdiff(ls(), c("all_data", "analyzed_folder", "file_name", "file_id",
                             "failed_files", "current_version", "is_already_analyzed",
                             "get_github_version", as.character(lsf.str()))))
}

print("ALL FILES ANALYZED")

if (length(failed_files) > 0) {
  message("\n--- FAILED FILES (", length(failed_files), ") ---")
  for (f in failed_files) message("  x ", f)
} else {
  message("\nNo failures detected.")
}