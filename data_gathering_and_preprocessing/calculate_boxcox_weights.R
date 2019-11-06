source("utils/preprocessing_utils.R")
load("data/data_final/data_final.RData")

# Calculate weights for weekly data
for (horizon in 1:6){
  set.seed(horizon)
  columns = c("Besucher_wochenende1_log", paste0("Woche", horizon, "_haupttitel"),
              paste0("Woche", horizon, "_haupttitel_film"), paste0("Woche", horizon, "_gesamttitel"), "Untertitel")
  data_sub = data_final[, columns]
  cat("Calculation for week", horizon, "with params:", paste0(colnames(data_sub), collapse = ", "), "\n")
  values = optim_weights(data = data_sub, n_outer = 3, n_inner = 10, reps_inner = 5)
  assign(paste0("weights", horizon), values)
  gc()
  save.image("best_weights.Rdata")
}

# Calculate weights for aggregated data
for (horizon in 1:6){
  set.seed(horizon)
  columns = c("Besucher_wochenende1_log", paste0("Aggregation", horizon, "_haupttitel"),
              paste0("Aggregation", horizon, "_haupttitel_film"), paste0("Aggregation", horizon, "_gesamttitel"),
              "Untertitel")
  data_sub = data_final[, columns]
  cat("Calculation for week", horizon, "with params:", paste0(colnames(data_sub), collapse = ","), "\n")
  values = optim_weights(data = data_sub, n_outer = 3, n_inner = 10, reps_inner = 5)
  assign(paste0("weights_agg",horizon), values)
  gc()
  save.image("best_weights.Rdata")
}

# Take weekly mean of the weights to get the final parameters for each linear comination
weights = matrix(ncol = 12, nrow = 2)
for(i in 1:6){
  current_weights = unlist(get(paste0("weights", i))[[1]])
  weights[1, 7 - i] = mean(current_weights[c(1, 3, 5)])
  weights[2, 7 - i] = mean(current_weights[c(2, 4, 6)])
  
  current_weights = unlist(get(paste0("weights_agg", i))[[1]])
  weights[1, 13 - i] = mean(current_weights[c(1, 3, 5)])
  weights[2, 13 - i] = mean(current_weights[c(2, 4, 6)])
}
rownames(weights) <- c("Gewichtung Haupttitel Film", "Gewichtung Haupt- und Untertitel")
colnames(weights) = c(paste0("Woche", 6:1), paste0("Aggregation", 6:1))
(weights = weights/99)

remove(weights1, weights2, weights3, weights4, weights5, weights6, weights_agg1, weights_agg2, weights_agg3,
       weights_agg4, weights_agg5, weights_agg6, i, calculate_box_cox, cross_validate_inner, estimate_box_cox,
       optim_weights, data_final, data_sub, lambda, current_weights, horizon, values, columns, calculate_google_value,
       calculate_median, define_searchterms, forecast_date, select_gt_data)

save.image("data/parameters/weights.Rdata")
