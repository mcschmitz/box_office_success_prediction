require(docstring)
require(matrixStats)
require(geoR)

forecast_date = function(date, forecast) {
  #' Function to calculcate the end date of the forecast
  #' 
  #' @param date [POSIXct]: Premier date
  #' @param forecast [numeric]: number of weeks before premier
  #'
  #'
  #' @return [POSIXct] enddate of the forecast. Always a saturday
  
  if(strftime(x = date, format = "%a", usetz = FALSE) == "Mi") {
    date = date - as.difftime(tim = 3 + (forecast - 1) * 7, units = "days")
  }
  
  if(strftime(x = date, format = "%a", usetz = FALSE) == "Do") {
    date = date - as.difftime(tim = 4 + (forecast - 1) * 7, units = "days")
  }
  return(date)
}

define_searchterms = function(term) {
  #' Function to generate search terms for Google Trends Data
  #' 
  #' @param term [char]: movie title 
  #' 
  #' @return Vector contraining 3 entries: Main title of the movie, main title of the movie  + suffix 'film'

  main_title = term
  main_title = strsplit(x = main_title, split = " - ")
  main_title = do.call(what = rbind, args = main_title)[, 1]
  main_title = strsplit(x = main_title, split = ": ")
  main_title = do.call(rbind, main_title)[, 1]
  main_title = gsub(pattern = "!", replacement = "", x = main_title)
  main_title = gsub(pattern = "\\?", replacement = "", x = main_title)
  main_title = gsub(pattern = "\\,", replacement = "", x = main_title)
  main_title = gsub(pattern = "\\(3D\\)", replacement = "", x = main_title)
  main_title = gsub(pattern = "3D", replacement = "", x = main_title)
  main_title = gsub(pattern = "\\.\\.\\.", replacement = "", x = main_title)
  main_title = gsub(pattern = "\\.\\.", replacement = "", x = main_title)

  main_title_film = paste(main_title, "film")

  complete_title = term
  complete_title = gsub(pattern = " - ", replacement = " ", x = complete_title)
  complete_title = gsub(pattern = "!", replacement = "", x = complete_title)
  complete_title = gsub(pattern = "\\?", replacement = "", x = complete_title)
  complete_title = gsub(pattern = "\\,", replacement = "", x = complete_title)

  complete_title = gsub(pattern = "\\(3D\\)", replacement = "", x = complete_title)
  complete_title = gsub(pattern = "3D", replacement = "", x = complete_title)
  complete_title = gsub(pattern = "\\.\\.\\.", replacement = "", x = complete_title)
  complete_title = gsub(pattern = "\\.\\.", replacement = "", x = complete_title)
  return(list("1_main_title" = main_title, "2_main_title_film" = main_title_film,
              "3_complete_title" = complete_title))
}

select_gt_data = function(trends, enddates, terms) {
  #' Function to extract the required weeks of data out of the Google Trends dataset
  #' 
  #' @param trends [data.frame]: Google Trends dataset
  #' @param enddates [Date]: Vector containting enddates of the forecast for the movies
  #' @param terms [character]: Movietitles
  #' 
  #' @return [data.frame]: data.frame containing 13 columns:
  #' - Spalte mit Filmtitel
  #' - 6 Spalten mit wöchentlichen Google-Daten
  #' - 6 Spalten mit aggregierten Google-Daten
  
  trends = as.matrix(trends[, -1])
  data = matrix(nrow = nrow(trends), ncol = 6)
  data_aggregated = matrix(nrow = nrow(trends), ncol = 6)
  for(i in seq_along(terms)) {
    end = which(as.character(enddates[i]) == colnames(trends))
    start = end - 5
    data[i, ] = trends[i, start:end]
  }
  # Set negative Google Values to zero
  data = apply(X = data, MARGIN = 2, FUN = function(x) {
    ifelse(test = x < 0, yes = 0, no = x)
  })
  data_aggregated[, 1] = data[, 1]
  for(j in 2:6) {
    data_aggregated[, j] = rowSums(data[, 1 : j])
  }
  # Prepare return value
  data = as.data.frame(data)
  data_aggregated = as.data.frame(data_aggregated)
  data = cbind(terms, data, data_aggregated)
  colnames(data) = c("Filmtitel", paste0("Woche", 6:1), paste0("Aggregation", 6:1))
  return(data)
}


calculate_median = function(trends, enddates, terms, frame = 52) {
  #' Function to calculate the median of the Google Treds data before the forecasting time window
  #' 
  #' @param trends [data.frame]: Google Trends Data
  #' @param enddates [POSIXt]: Enddates of the forecast horizon
  #' @param terms [chracter]: movietitles
  #' @param frame: [numeric]: timeframe that should be used to calculate the median
  #' 
  #' @return Median of the Google Trends data for each movie
  
  trends = as.matrix(trends[, -1])
  data = matrix(nrow = nrow(trends), ncol = frame)
  for(i in seq_along(terms)) {
    end = which(as.character(enddates[i]) == colnames(trends)) - 6
    start = end - (frame - 1)
    data[i, ] = trends[i, start:end]
  }
  median = rowMedians(data)
  return(median)
}

calculate_google_value = function(data, weights) {
  #' Function to calculate the Google Value
  #' 
  #' @param data [data.frame]: Google Trends data
  #' @param weights [numeric]: Weights to calculate the Google Value
  #' 
  #' @return google_value [numeric]: Google-Values
  
  google_values = matrix(data = NA, nrow = nrow(data), ncol = 12)

    for(i in 1:6) {
    columns = head(grep(pattern = paste0("Woche", i), x = colnames(data)), 3)
    data_sub = data[, columns]
    weights_sub = weights[, 7 - i]
    google_values[, 7 - i] = weights_sub[1] * data_sub[, 1] + 
      (1 - weights_sub[1]) * data_sub[, 3]
    for(j in 1:nrow(data_sub)) {
      # If the title has a subtitle add an additional linear combination based first linear combination and the main 
      # title
      if(data_sub[j, 1] != data_sub[j, 2]) {
        google_values[j, 7 - i] = weights_sub[2] * google_values[j, 7 - i] + (1 - weights_sub[2]) * data_sub[j, 2]
      }
    }
  }

    for(i in 1:6) {
    columns = head(grep(pattern = paste0("Aggregation", i),
                        x = colnames(data)), 3)
    data_sub = data[, columns]
    weights_sub = weights[, 13 - i]
    # Linear comination of main title and main title + film
    google_values[, 13 - i] = weights_sub[1] * data_sub[, 1] + 
      (1 - weights_sub[1]) * data_sub[, 3]
    for(j in 1:nrow(data_sub)) {
      if(data_sub[j, 1] != data_sub[j, 2]) {
        google_values[j, 13 - i] = weights_sub[2] * google_values[j, 13 - i] + (1 - weights_sub[2]) * data_sub[j, 2]
      }
    }
  }  
  google_values = as.data.frame(google_values)
  colnames(google_values) = c(paste0("Woche", 6:1), paste0("Aggregation", 6:1))
  return(google_values)
}

estimate_box_cox = function(vector) {
  #' Apply Box-Cox Transformation
  #' 
  #' @param [numeric] Input data
  #' 
  #' @return [list] google_value [numeric]: tranformed input, lambda [numeric]: estimated Lambda
  
  lambda = boxcoxfit(object = vector, lambda2 = TRUE,
                     method = "L-BFGS-B")$lambda
  
  if(is.null(lambda[1])) {
    vector = log(vector + lambda[2])
  }
  else {
    vector = ((vector + lambda[2]) ^ lambda[1] - 1) / lambda[1]
  }
  return(list("google_value" = vector, "lambda" = lambda))
}



