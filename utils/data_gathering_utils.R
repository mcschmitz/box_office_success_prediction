require(docstring)
require(gtrendsR)
require(RCurl)
require(Hmisc)
require(chron)
require(reshape2)
require(matrixStats)


export_gt_data <- function(terms, startdates, enddates = NULL, geo = "DE", anker, ...) {
  #' Function to gather Google Trends data
  #' 
  #' @param terms [character]: Search terms
  #' @param startdates [POSIXt]: startdates for data gathering of the terms
  #' @param enddates [POSIXt]: enddates for data gathering of the terms. Default is today's date.
  #' @param geo [character]:Geographical searchspace for data gathering
  #' @param anker [character]: Ankerterm
  #' @param ... Additional arguments
  #' 
  #' @return trends [data.frame]: Gathered data
  
  if(is.null(enddates)){
    enddates <- Sys.Date()
    enddates <- as.POSIXct(enddates)
    enddates <- trunc(enddates, "months")
    enddates <- trunc(enddates, "days") - 24*3600
  }
  enddates_upper <- ceil(enddates, "months")
  startdates_lower <- trunc(startdates, "months")
  stopifnot(is.character(terms))
  stopifnot(is.character(anker))
  
  # Split termms in chunks
  terms <- terms[nchar(terms) > 0]
  terms <- gsub(x = terms, pattern = ",", "")
  nchunks <- ceiling(length(terms)/4)
  split_factor <- rep(1:nchunks, each = 4)
  split_factor <- split_factor[1:length(terms)]
  terms_list <- split(terms, split_factor)
  
  trends <- list()
  pb <- txtProgressBar(min = 0, max = length(terms_list), style = 3)
  
  # Data gathering. sleep commmands are needed to prevent banning
  for(i in seq_along(terms_list)){
    current_terms <- c(anker, terms_list[[i]])
    Sys.sleep(0.2)
    time_frame <- paste(startdates_lower, enddates_upper)
    gt_output <- try(gtrends(current_terms, time = time_frame, onlyInterest = TRUE, geo = geo), silent = TRUE)
    gt_output <- gt_output$interest_over_time
    Sys.sleep(0.2)
    closeAllConnections()
    # Error-Log
    if(inherits(gt_output, "try-error")) {
      trends[[i]] <- NA
      cat("\n", gt_output, "for", current_terms)
    }
    else {
      current_terms_output <- enc2utf8(unique(gt_output$keyword))
      weeks <- nrow(gt_output)/length(current_terms)
      gt_output$id <- rep(1:weeks,times = length(unique(gt_output$keyword)))
      gt_output$hits <- as.numeric(gt_output$hits)
      gt_output$hits[is.na(gt_output$hits)] <- 0
      gt_casted <- dcast(data = gt_output,formula = id ~ keyword, fun.aggregate = sum,value.var = "hits")
      
      gt_casted$start <- gt_output$date[1:weeks]
      gt_casted$end <- c(gt_output$date[2:weeks], tail(gt_output$date, 1) + as.difftime(tim = 7, units = "days"))
      gt_casted$end <- as.Date(gt_casted$end, format = "%Y-%m%-%d", tz = "GMT")
      gt_casted <- gt_casted[, c("start", "end", current_terms_output)]
      colnames(gt_casted) <- c("start", "end", current_terms)
      trends[[i]] <- cbind(gt_casted[, 1:2], gt_casted[, 3:ncol(gt_casted)] / gt_casted[, 3])
      
      if(i > 1){
        trends[[i]] <- trends[[i]][, 4:ncol(trends[[i]])]
      }
      else {
        trends[[i]] <- trends[[i]]
      }
      Sys.sleep(runif(1, 1, 3))
    }
    setTxtProgressBar(pb, i)
    if (i != length(terms_list)) {
    }
    
    if (i %% 25 == 0){
      options(HTTPUserAgent <- paste(paste(LETTERS[round(runif(3, 1, 24))], sep = "", collapse = ""), 
                                    "(3.2.3 x86_64-w64-mingw32 x86_64 mingw32)"))
    }
  }
  options(HTTPUserAgent <- paste(paste(LETTERS[round(runif(3, 1, 24))], sep = "", collapse = ""), 
                                "(3.2.3 x86_64-w64-mingw32 x86_64 mingw32)"))
  
  trends <- do.call(cbind, trends)
  return(trends)
}

detect_low_level_terms <- function(terms, trends, enddate) {
  #' Function to detect movies which have to be compared to the next lower anker term
  #' 
  #' @param terms [chracter]: Search terms
  #' @param trends [data.frame]: Data Frame containing Google trends data
  #' @param enddate [POSIXt]: enddates for data gathering of the terms
  #' 
  #' @return google_value [numeric]: Google trend data
  
  trends_frame <- list()
  
  for(i in 4:ncol(trends)) {
    end <- which(as.character(trends[ ,2]) == as.character(enddate[i-3]))
    start <- end - 5
    trends_frame[[i]] <- trends[start:end, i]
  }
  trends_frame <- do.call(cbind, trends_frame)
  
  c_max <- colMaxs(trends_frame)
  terms_low_level <- ifelse(test = c_max < 0.25, yes = terms, no = NA)
  id_low_level <- which(!(is.na(terms_low_level)))
  id_high_level <- which(is.na(terms_low_level))
  
  return(list("low" = id_low_level, "high" = id_high_level))
}

scale_down <- function(upper_gt, lower_gt, scale_gt) {
  #' Function to scale down Google Trends data
  #' 
  #' @param upper_gt [data.frame]: Gooogle Trends-Daten with top level anker
  #' @param lower_gt [data.frame]: Gooogle Trends-Daten mit low level anker
  #' @param scale_gt [numeric]: Scaling
  #' 
  #' @return [data.frame]: Scaled Google Trends data
  
  names_upper <- colnames(upper_gt[, -c(1:3)])
  names_lower <- colnames(lower_gt[, -c(1:3)])
  lower_gt <- lower_gt[, -c(1:3)]
  
  scale_id <- which(names_upper %in% names_lower)
  for (i in seq_along(scale_id)){
    upper_gt[, (scale_id[i] + 3)] <- lower_gt[, i] * scale_gt
  }
  return(upper_gt)
}

