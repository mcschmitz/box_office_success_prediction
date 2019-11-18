# Load Google Trends data
load("data/gt-data_anker/gt-data_anker_final.RData")
load("data/movies_preprocessed.RData")

# Load weights for the linear comination of the Google Trends search terms
load("data/parameters/weights.RData")

# Source functions
source("utils/preprocessing_utils.R")

startdates = as.Date(movies$Prognose_start)
enddates = as.Date(movies$Prognosedatum1)
films = movies$Filmtitel

# Main Title ===========================================================================================================
# Filter for median before forecasting window
row_median = calculate_median(trends = gt_data_main_title, enddates = enddates, terms = films, frame = 52)
row_median[is.na(row_median)] = 0
gt_data_main_title[, -1] = gt_data_main_title[, -1] - row_median

# Filter for relevant Google Trends Data
main_title_final = select_gt_data(trends = gt_data_main_title, enddates = enddates, terms = films)
colnames(main_title_final) = c("Filmtitel", paste0("Woche", 6:1, "_haupttitel"), 
                               paste0("Aggregation", 6:1, "_haupttitel"))
head(main_title_final)


# Complete Title =======================================================================================================
row_median <- calculate_median(trends = gt_data_complete_title, enddates = enddates, terms = films, frame = 52)
row_median[is.na(row_median)] = 0
gt_data_complete_title[, -1] = gt_data_complete_title[, -1] - row_median

complete_title_final = select_gt_data(trends = gt_data_complete_title, enddates = enddates, terms = films)
colnames(complete_title_final) = c("Filmtitel", paste0("Woche", 6:1, "_gesamttitel"),
                                    paste0("Aggregation", 6:1, "_gesamttitel"))
head(complete_title_final)

# Main Title + film ====================================================================================================
row_median <- calculate_median(trends = gt_data_main_title_film, enddates = enddates, terms = films, frame = 52)
row_median[is.na(row_median)] = 0
gt_data_main_title_film[, -1] = gt_data_main_title_film[, -1] - row_median

main_title_film_final = select_gt_data(trends = gt_data_main_title_film, enddates = enddates, terms = films)
colnames(main_title_film_final) = c("Filmtitel", paste0("Woche", 6:1, "_haupttitel_film"),
                                    paste0("Aggregation", 6:1, "_haupttitel_film"))
head(main_title_film_final)


# Add Google Trends data for search term "Kino"
kino_data = matrix(nrow = nrow(movies), ncol = 6)
for(i in seq_along(movies$Prognosedatum1)) {
  end = which(as.character(enddates[i]) == gt_kino[,1])
  start = end - 5
  kino_data[i, ] = as.integer(gt_kino$hits[start:end])
}
kino_data = as.data.frame(kino_data)
colnames(kino_data) = c(paste0("Woche", 6:1, "_kino"))

# Concatenate data
data_final = cbind(movies, main_title_final[, -1], complete_title_final[, -1], main_title_film_final[, -1], kino_data)

# Remove data with infinte values
data_final = data_final[-c(369, 383), ]

# Calculate Google Values
weights = as.data.frame(weights)
google_values = calculate_google_value(data = data_final, weights = weights)
data_final = cbind(data_final, google_values)

# Calculate Box-Cox transformation
google_values = data_final[, 102:113]
lambda = matrix(nrow = 2, ncol = 12)
for(i in 1:ncol(google_values)) {
  boxcox = estimate_box_cox(google_values[, i])
  google_values[, i] = boxcox$google_value
  lambda[, i] = boxcox$lambda
}
colnames(google_values) = paste0(colnames(google_values), "_transformiert")
lambda = as.data.frame(lambda)
colnames(lambda) = colnames(google_values)
rownames(lambda) = c("lambda1", "lambda2")

# Add Box-Cox transformed Google Values
data_final = cbind(data_final, google_values)

# Abspeichern des finalen Workspaces:
rm(list = ls()[which(!(ls() %in% c("data_final", "lambda", "weights")))])
save.image("data/data_final/data_final.RData")
