require(docstring)

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

  complete_title = gsub(pattern = "\\(3D\\)", replacement = "",
                        x = complete_title)
  complete_title = gsub(pattern = "3D", replacement = "", x = complete_title)
  complete_title = gsub(pattern = "\\.\\.\\.", replacement = "",
                        x = complete_title)
  complete_title = gsub(pattern = "\\.\\.", replacement = "",
                        x = complete_title)
  return(list("1_main_title" = main_title, "2_main_title_film" = main_title_film,
              "3_complete_title" = complete_title))
}
