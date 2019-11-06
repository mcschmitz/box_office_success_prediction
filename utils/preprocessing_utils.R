require(docstring)
require(matrixStats)
require(parallel)
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
  #' @param vector [numeric] Input data
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


calculate_box_cox = function(google_bucket, lambda) {
  #' Calculate Box-Cox Transformation on a vector
  #' 
  #' @param google_bucket [numeric]: vector that should be transformed
  #' @param lambda [numeric]: transformation weights
  #' 
  #' @return [numeric] transformed vector
  
  trafo = c()
  for(i in 1:length(google_bucket)){
    lambda_sub = lambda
    if(lambda_sub[1] == 0) {
      trafo[i] = log(google_bucket[i] + lambda_sub[2])
    }
    else{
      trafo[i] = ((google_bucket[i] + lambda_sub[2]) ^ lambda_sub[1] - 1) /
        lambda_sub[1]
    }
  }
  names(trafo) = names(google_bucket)
  return(trafo)
}

optim_weights = function(data, n_outer = 3, n_inner = 10, reps_inner = 5){
  #' Function to optimize weights via nested resampling
  #' 
  #' @param data [data.frame]
  #' @param n_outer [numeric]: k for outer k-fold CV
  #' @param n_innter [numeric]: k for inner k-fold CV
  #' @param reps_inner [numeric]: number of repititions for inner CV
  #' 
  #' @return best_params [numeric]: best weight combination
  #' @return correlations [numeric]: correlations of the single CV folds

  w_m = seq(0.01, 0.99, by = 0.01)
  w_mc = seq(0.01, 0.99, by = 0.01)
  i = 0
  n_folds_outer = n_outer
  n_folds_inner = n_inner
  n_reps_inner = reps_inner
  best_params = list()
  lambda = list()
  correlations = c()

  folds_outer = sample(rep(1:n_folds_outer, length.out = nrow(data)))
  n_cores = min(reps_inner, (detectCores() - 1))

  for(outer_fold in 1:n_folds_outer) {
    train_set_outer = data[which(folds_outer != outer_fold), ]
    test_set_outer = data[which(folds_outer == outer_fold), ]
    inner_result = matrix(nrow = length(w_mc), ncol = length(w_m))
    samples_inner = replicate(n = n_reps_inner, expr = {
      sample(rep(1:n_folds_inner, length.out = nrow(train_set_outer)))},
      simplify = FALSE)
    pb = txtProgressBar(min = 0, max = n_outer * length(w_m), style = 3)
    cl = makePSOCKcluster(n_cores)

    for(m in 1:length(w_m)) {
      for(mc in 1:length(w_mc)) {
        clusterExport(cl, varlist = list("cross_validate_inner", "n_folds_inner", "train_set_outer", "m", "mc",
                                         "estimate_box_cox", "boxcoxfit", "calculate_box_cox"),
                      envir = environment())

        cors_reps = parLapply(cl, fun = cross_validate_inner, X = samples_inner, n_folds_inner = n_folds_inner,
                              data = train_set_outer, w_m = w_m[m], w_mc = w_mc[mc])
        inner_result[m, mc] = mean(unlist(cors_reps))
        gc()
      }
      i = i + 1
      setTxtProgressBar(pb, i)
    }
    stopCluster(cl)
    gc()

    best_params[[outer_fold]] = which(inner_result == max(inner_result), arr.ind = TRUE)
    best_m = best_params[[outer_fold]][1]
    best_mc = best_params[[outer_fold]][2]

    bucket_m = w_m[best_m] * train_set_outer[, 2] + (1-w_m[best_m]) * train_set_outer[, 3]
    bucket = ifelse(train_set_outer[, 5] == 1, 
                    w_mc[best_mc] * bucket_m + (1 - w_mc[best_mc]) * train_set_outer[, 4],
                    bucket_m)
    trafo_result = estimate_box_cox(bucket)
    lambda[outer_fold] = trafo_result$lambda
    
    t_bucket_m_outer = w_m[best_m] * test_set_outer[, 2] + (1-w_m[best_m]) * test_set_outer[, 3]
    t_bucket_outer = ifelse(test_set_outer[, 5] == 1, 
                            w_mc[best_mc] * t_bucket_m_outer + (1 - w_mc[best_mc]) * test_set_outer[, 4],
                            t_bucket_m_outer)
    t_bucket_outer_trafo = calculate_box_cox(t_bucket_outer, trafo_result$lambda)
    correlations[outer_fold] = cor(t_bucket_outer_trafo, test_set_outer[, 1])
  }
  return(list(best_params, correlations))
}

# Funktion zur Durchfuerhrung der inneren CV Schleife
cross_validate_inner = function(data, sample, n_folds_inner, w_m, w_mc) {
  #' Utility function to apply inner CV Loop
  #' 
  #' @param data [data.frame]: dataset
  #' @param sample [numeric]: sample indeces
  #' @param n_volds_inner [numeric]: Number of inner CVs
  #' @param w_m [numeric]: weight used for linear combination of main title and main title + film
  #' @param w_mc [numeric]: weight used for linear combination between first linear combinations and the complete title
  #' 
  #' @return [numeric]: mean of correlations on the test datasets of the CV

  folds_inner = sample
  mean_corrs_inner = c()
  correlations_inner = c()

  for(inner_fold in 1:n_folds_inner) {
    train_set_inner = data[which(folds_inner != inner_fold), ]
    test_set_inner = data[which(folds_inner == inner_fold), ]

    bucket_m_inner = w_m * train_set_inner[, 2] + (1-w_m) * train_set_inner[, 3]
    bucket_inner = ifelse(train_set_inner[, 5] == 1, 
                          w_mc * bucket_m_inner + (1 - w_mc) * train_set_inner[, 4],
                          bucket_m_inner)
    trafo_result_inner = estimate_box_cox(bucket_inner)
    
    t_bucket_m_inner = w_m * test_set_inner[, 2] + (1-w_m) * test_set_inner[, 3]
    t_bucket_inner = ifelse(test_set_inner[, 5] == 1, 
                            w_mc * t_bucket_m_inner + (1 - w_mc) * test_set_inner[, 4],
                            t_bucket_m_inner)
    t_bucket_inner_trafo = calculate_box_cox(t_bucket_inner, trafo_result_inner$lambda)

    correlations_inner = append(correlations_inner, values = cor(t_bucket_inner_trafo, test_set_inner[, 1]))
  }
  mean_corrs_inner = mean(correlations_inner)
  return(mean_corrs_inner)
}


