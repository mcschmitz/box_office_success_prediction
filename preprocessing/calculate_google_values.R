load("data/concatenated_data.RData")
source("utils/preprocessing_utils.R")

# training sample
train_size <- floor(0.75 * nrow(data_final))
set.seed(101)
train_idx <- sample(seq_len(nrow(data_final)), size = train_size)

train <- data_final[train_idx, ]
test <- data_final[-train_idx, ]

# Calculate weights for weekly data
for (horizon in 1:6){
  set.seed(horizon)
  columns <- c("visitors_premiere_weekend_log", paste0("week", horizon, "_main_title"),
              paste0("week", horizon, "_main_title_film"), paste0("week", horizon, "_complete_title"), 
              "secondary_title")
  data_sub <- train[, columns]
  cat("Calculation for week", horizon, "with params:", paste0(colnames(data_sub), collapse = ", "), "\n")
  values <- optim_weights(data = data_sub, n_outer = 10, n_inner = 10)
  assign(paste0("weights", horizon), values)
  gc()
  save.image("results/parameters/weights.Rdata")
}

# Calculate weights for aggregated data
for (horizon in 1:6){
  set.seed(horizon)
  columns <- c("visitors_premiere_weekend_log", paste0("aggregation", horizon, "_main_title"),
              paste0("aggregation", horizon, "_main_title_film"), paste0("aggregation", horizon, "_complete_title"),
              "secondary_title")
  data_sub <- train[, columns]
  cat("Calculation for week", horizon, "with params:", paste0(colnames(data_sub), collapse = ","), "\n")
  values <- optim_weights(data = data_sub, n_outer = 10, n_inner = 10)
  assign(paste0("weights_agg",horizon), values)
  gc()
  save.image("results/parameters/weights.Rdata")
}

# Take weekly mean of the weights to get the final parameters for each linear comination
weights <- matrix(ncol = 12, nrow = 2)
for(i in 1:6){
  current_weights <- unlist(get(paste0("weights", i))[[1]])
  weights[1, 7 - i] <- mean(current_weights[seq(1, length(current_weights), 2)])
  weights[2, 7 - i] <- mean(current_weights[1 + seq(1, length(current_weights), 2)])
  
  current_weights <- unlist(get(paste0("weights_agg", i))[[1]])
  weights[1, 13 - i] <- mean(current_weights[seq(1, length(current_weights), 2)])
  weights[2, 13 - i] <- mean(current_weights[1 + seq(1, length(current_weights), 2)])
}
rownames(weights) <- c("weights_main_title_film", "weights_complete_title")
colnames(weights) <- c(paste0("week", 6:1), paste0("aggregation", 6:1))

save.image("results/parameters/weights.Rdata")

# Calculate Google Values
weights <- as.data.frame(weights)
google_values <- calculate_google_value(data = train, weights = weights)
train <- cbind(train, google_values)

# Calculate Box-Cox transformation
columns <- c(paste0("week", 1:6), paste0("aggregation", 1:6))
google_values <- train[, columns]
lambda <- matrix(nrow = 2, ncol = 12)
for(i in 1:ncol(google_values)) {
  boxcox <- estimate_box_cox(google_values[, i])
  google_values[, i] <- boxcox$google_value
  lambda[, i] <- boxcox$lambda
}
colnames(google_values) <- paste0(colnames(google_values), "_transformiert")
lambda <- as.data.frame(lambda)
colnames(lambda) <- colnames(google_values)
rownames(lambda) <- c("lambda1", "lambda2")
train <- cbind(train, google_values)

# Apply boxcox trafo on test test
google_values <- calculate_google_value(data = test, weights = weights)
test <- cbind(test, google_values)
google_values <- test[, columns]
for(i in 1:ncol(google_values)) {
  boxcox <- calculate_box_cox(google_values[, i], lambda[, i])
  google_values[, i] <- boxcox
}
colnames(google_values) <- paste0(colnames(google_values), "_transformiert")
test <- cbind(test, google_values)

# Apply boxcox trafo complete data test
google_values <- calculate_google_value(data = data_final, weights = weights)
data_final <- cbind(data_final, google_values)
google_values <- data_final[, columns]
for(i in 1:ncol(google_values)) {
  boxcox <- calculate_box_cox(google_values[, i], lambda[, i])
  google_values[, i] <- boxcox
}
colnames(google_values) <- paste0(colnames(google_values), "_transformiert")
data_final <- cbind(data_final, google_values)

# save final workspace
rm(list = ls()[which(!(ls() %in% c("data_final", "train", "test", "lambda", "weights")))])
save.image("data/data_final.RData")
