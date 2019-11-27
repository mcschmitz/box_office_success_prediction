require(docstring)
require(matrixStats)
require(parallel)
require(geoR)
require(mlrMBO)
require(purrr)

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
  colnames(data) = c("title", paste0("week", 6:1), paste0("aggregation", 6:1))
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
    columns = head(grep(pattern = paste0("week", i), x = colnames(data)), 3)
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
    columns = head(grep(pattern = paste0("aggregation", i),
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
  colnames(google_values) = c(paste0("week", 6:1), paste0("aggregation", 6:1))
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

optim_weights = function(data, n_outer = 3, n_inner = 10){
  #' Function to optimize weights via nested resampling
  #' 
  #' @param data [data.frame]
  #' @param n_outer [numeric]: k for outer k-fold CV
  #' @param n_innter [numeric]: k for inner k-fold CV
  #' @param reps_inner [numeric]: number of repititions for inner CV
  #' 
  #' @return best_params [numeric]: best weight combination
  #' @return correlations [numeric]: correlations of the single CV folds

  n_folds_outer = n_outer
  n_folds_inner = n_inner
  best_params = list()
  lambda = list()
  correlations = c()

  folds_outer <- cut(seq(1,nrow(data)), breaks = n_outer, labels = FALSE)

  for(fold in 1:n_folds_outer) {
    testIndexes <- which(folds_outer == fold,arr.ind = TRUE)
    train = data[-testIndexes, ]
    test = data[testIndexes, ]
    
    f = partial(cross_validate_inner, data = train, n_inner = n_folds_inner)
    obj.fun = makeSingleObjectiveFunction(
      name = "corr_optimization",
      fn = f,
      par.set = makeParamSet(
        makeNumericParam("main_title_weight", lower = 0, upper = 1),
        makeNumericParam("complete_title_weight", lower = 0, upper = 1)
      ),
      minimize = FALSE
    )
    des = generateDesign(n = 8, par.set = getParamSet(obj.fun), fun = lhs::randomLHS)
    des$y = apply(des, 1, obj.fun)
    surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
    control = makeMBOControl()
    control = setMBOControlTermination(control, iters = 80)
    control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())
    run = mbo(obj.fun, design = des, learner = surr.km, control = control, show.info = FALSE)
    best_m = run$x$main_title_weight
    best_mc = run$x$complete_title_weight

    bucket_m = best_m * train[, 2] + (1-best_m) * train[, 3]
    bucket = ifelse(train[, 5] == 1, 
                    best_mc * bucket_m + (1 - best_mc) * train[, 4],
                    bucket_m)
    trafo_result = estimate_box_cox(bucket)
    lambda$fold = trafo_result$lambda
    
    t_bucket_m_outer = best_m * test[, 2] + (1-best_m) * test[, 3]
    t_bucket_outer = ifelse(test[, 5] == 1, 
                            best_mc * t_bucket_m_outer + (1 - best_mc) * test[, 4],
                            t_bucket_m_outer)
    t_bucket_outer_trafo = calculate_box_cox(t_bucket_outer, trafo_result$lambda)
    correlations[fold] = cor(t_bucket_outer_trafo, test[, 1])
    best_params[[fold]] = c(best_m, best_mc)
  }
  return(list(best_params, correlations))
}


cross_validate_inner = function(data, x, n_inner) {
  #' Utility function to apply inner CV Loop
  #' 
  #' @param data [data.frame]: dataset
  #' @param sample [numeric]: sample indeces
  #' @param n_volds_inner [numeric]: Number of inner CVs
  #' @param w_m [numeric]: weight used for linear combination of main title and main title + film
  #' @param w_mc [numeric]: weight used for linear combination between first linear combinations and the complete title
  #' 
  #' @return [numeric]: mean of correlations on the test datasets of the CV

  w_m = x[1]
  w_mc = x[2]
  avg_correlations = c()
  folds <- cut(seq(1,nrow(data)), breaks = n_inner, labels = FALSE)
  for (i in 1:n_inner){
    testIndexes <- which(folds == i,arr.ind = TRUE)
    train <- data[-testIndexes, ]
    test <- data[testIndexes, ]
    
    bucket_m_inner = w_m * train[, 2] + (1-w_m) * train[, 3]
    bucket_inner = ifelse(train[, 5] == 1, 
                          w_mc * bucket_m_inner + (1 - w_mc) * train[, 4],
                          bucket_m_inner)
    trafo_result_inner = estimate_box_cox(bucket_inner)
    
    t_bucket_m_inner = w_m * test[, 2] + (1-w_m) * test[, 3]
    t_bucket_inner = ifelse(test[, 5] == 1, 
                            w_mc * t_bucket_m_inner + (1 - w_mc) * test[, 4],
                            t_bucket_m_inner)
    t_bucket_inner_trafo = calculate_box_cox(t_bucket_inner, trafo_result_inner$lambda)
    avg_correlations[i] = cor(t_bucket_inner_trafo, test[, 1])
  }
  return(mean(avg_correlations, na.rm=TRUE))
}


