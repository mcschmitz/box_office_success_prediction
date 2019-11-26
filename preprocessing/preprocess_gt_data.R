# Load Google Trends data
load("data/gt_data_complete.RData")
load("data/movies_preprocessed.RData")

# Source functions
source("utils/preprocessing_utils.R")

startdates <-as.Date(movies$forecast_start)
enddates <- as.Date(movies$forecast_date1)

# Main Title ===========================================================================================================
# Filter for median before forecasting window
row_median <- calculate_median(trends = gt_data_main_title, enddates = enddates, 
                               terms = gt_data_main_title$title, frame = 52)
row_median[is.na(row_median)] <- 0
gt_data_main_title[, -1] <- gt_data_main_title[, -1] - row_median

# Filter for relevant Google Trends Data
main_title_final <- select_gt_data(trends = gt_data_main_title, enddates = enddates, terms = gt_data_main_title$title)
colnames(main_title_final) <- c("title", paste0("week", 6:1, "_main_title"), 
                               paste0("aggregation", 6:1, "_main_title"))
head(main_title_final)

# Complete Title =======================================================================================================
row_median <- calculate_median(trends = gt_data_complete_title, enddates = enddates, 
                               terms = gt_data_complete_title$title, frame = 52)
row_median[is.na(row_median)] <- 0
gt_data_complete_title[, -1] <- gt_data_complete_title[, -1] - row_median

complete_title_final = select_gt_data(trends = gt_data_complete_title, enddates = enddates, 
                                      terms = gt_data_complete_title$title)
colnames(complete_title_final) <- c("title", paste0("week", 6:1, "_complete_title"),
                                    paste0("aggregation", 6:1, "_complete_title"))
head(complete_title_final)

# Main Title + film ====================================================================================================
row_median <- calculate_median(trends = gt_data_main_title_film, enddates = enddates, 
                               terms = gt_data_main_title_film$title, frame = 52)
row_median[is.na(row_median)] <- 0
gt_data_main_title_film[, -1] <- gt_data_main_title_film[, -1] - row_median

main_title_film_final <- select_gt_data(trends = gt_data_main_title_film, enddates = enddates, 
                                        terms = gt_data_main_title_film$title)
colnames(main_title_film_final) <- c("title", paste0("week", 6:1, "_main_title_film"),
                                    paste0("aggregation", 6:1, "_main_title_film"))
head(main_title_film_final)


# Add Google Trends data for search term "Kino"
kino_data <- matrix(nrow = nrow(movies), ncol = 6)
for(i in seq_along(movies$forecast_date1)) {
  end <- which(as.character(enddates[i]) == gt_kino[,1])
  start <- end - 5
  kino_data[i, ] <- as.integer(gt_kino$hits[start:end])
}
kino_data <- as.data.frame(kino_data)
colnames(kino_data) <- c(paste0("week", 6:1, "_kino"))

# Concatenate data
data_final <- cbind(movies, main_title_final[, -1], complete_title_final[, -1], main_title_film_final[, -1], kino_data)

rm(list = ls()[which(!(ls() %in% c("data_final")))])
save.image("data/concatenated_data.RData")