load("data/movies_preprocessed.RData")
load("data/gt_data_ankers.RData")
source("utils/data_gathering_utils.R")

# Gather data for the main title of the movie  ========================================================================
terms <- main_title
levels_idx <- 1:length(terms)
for (i in 1:length(ankers)){
  cat("Collecting data for anker term" ,i, "of", length(ankers),"\n")
  if (length(terms) > 0){
    # Pull Google Trends data with current anker
    x <- export_gt_data(terms = terms, startdates = startdate, enddates = enddate, anker = ankers[i])
    colnames(x)[4:ncol(x)] <- gsub(terms, pattern = " ", replacement = "_")
    
    last_forecast_date <- last_forecast_date[levels_idx]
    # Detect terms that have to be redrawn
    levels <- detect_low_level_terms(terms = terms, trends = x, enddate = last_forecast_date)
    levels_idx <- levels$low
    terms <- terms[levels_idx]
    
    assign(paste0("gt_data", i), x)
    
    save.image("data/gt_data_main_title.RData")
  }
}

# Scale down search terms
x <- scale_down(upper_gt = gt_data1, lower_gt = gt_data1, scale_gt = gt_anker1_median)
for (i in 2:length(ankers)){
  x <- scale_down(upper_gt = x, lower_gt = get(paste0("gt_data", i)), scale_gt = get(paste0("gt_anker1_anker", i)))
}

# Build final Google Trends dataset
gt_data_main_title <- x
gt_data_main_title <- as.data.frame(t(gt_data_main_title[,4:ncol(gt_data_main_title)]))
colnames(gt_data_main_title) <- gt_data1[, 2]
rownames(gt_data_main_title) <- 1:nrow(gt_data_main_title)
gt_data_main_title <- cbind("title" = movies$title, gt_data_main_title)
head(gt_data_main_title)
rm(list = ls() [which(ls() != "gt_data_main_title")])
save.image("data/gt_data_main_title.RData")


# Gather data for the main title  + film  =============================================================================
load("data/movies_preprocessed.RData")
load("data/gt_data_ankers.RData")
source("utils/data_gathering_utils.R")

terms <- main_title_film
levels_idx <- 1:length(terms)
for (i in 1:length(ankers)){
  cat("Collecting data for anker term" ,i, "of", length(ankers),"\n")
  if (length(terms) > 0){
    x <- export_gt_data(terms = terms, startdates = startdate, enddates = enddate, anker = ankers[i])
    colnames(x)[4:ncol(x)] <- gsub(terms, pattern = " ", replacement = "_")
    
    last_forecast_date <- last_forecast_date[levels_idx]
    levels <- detect_low_level_terms(terms = terms, trends = x, enddate = last_forecast_date)
    levels_idx <- levels$low
    terms <- terms[levels_idx]
    
    assign(paste0("gt_data", i), x)
    
    save.image("data/gt_data_main_title_film.RData")
  }
}

# Scale down search terms
x <- scale_down(upper_gt = gt_data1, lower_gt = gt_data1, scale_gt = gt_anker1_median)
for (i in 2:length(ankers)){
  x <- scale_down(upper_gt = x, lower_gt = get(paste0("gt_data", i)), scale_gt = get(paste0("gt_anker1_anker", i)))
}

# Build final Google Trends dataset
gt_data_main_title_film <- x
gt_data_main_title_film <- as.data.frame(t(gt_data_main_title_film[,4:ncol(gt_data_main_title_film)]))
colnames(gt_data_main_title_film) <- gt_data1[, 2]
rownames(gt_data_main_title_film) <- 1:nrow(gt_data_main_title_film)
gt_data_main_title_film <- cbind("title" = movies$title, gt_data_main_title_film)
head(gt_data_main_title_film)
rm(list = ls() [which(ls() != "gt_data_main_title_film")])
save.image("data/gt_data_main_title_film.RData")


# Gather data for the complete title  =================================================================================
load("data/movies_preprocessed.RData")
load("data/gt_data_ankers.RData")
source("utils/data_gathering_utils.R")

terms <- complete_title[which(main_title != complete_title)]
last_forecast_date <- last_forecast_date[which(main_title != complete_title)]
levels_idx <- 1:length(terms)
for (i in 1:length(ankers)){
  cat("Collecting data for anker term" ,i, "of", length(ankers),"\n")
  if (length(terms) > 0){
    x <- export_gt_data(terms = terms, startdates = startdate, enddates = enddate, anker = ankers[i])
    colnames(x)[4:ncol(x)] <- gsub(terms, pattern = " ", replacement = "_")
    
    last_forecast_date <- last_forecast_date[levels_idx]
    levels <- detect_low_level_terms(terms = terms, trends = x, enddate = last_forecast_date)
    levels_idx <- levels$low
    terms <- terms[levels_idx]
    
    assign(paste0("gt_data", i), x)
    
    save.image("data/gt_data_complete_title.RData")
  }
}

# Scale down search terms
x <- scale_down(upper_gt = gt_data1, lower_gt = gt_data1, scale_gt = gt_anker1_median)
for (i in 2:length(ankers)){
  x <- scale_down(upper_gt = x, lower_gt = get(paste0("gt_data", i)), scale_gt = get(paste0("gt_anker1_anker", i)))
}

# Build final Google Trends dataset
gt_data_complete_title <- x
gt_data_complete_title <- as.data.frame(t(gt_data_complete_title[,4:ncol(gt_data_complete_title)]))
rownames(gt_data_complete_title) <- 1:nrow(gt_data_complete_title)
gt_data_complete_title <- cbind("title" = movies$title[which(main_title != complete_title)], gt_data_complete_title)

load("data/gt_data_main_title.RData")
colnames(gt_data_complete_title) <- colnames(gt_data_main_title)
gt_data_complete_title = rbind(gt_data_main_title[which(complete_title == main_title), ], gt_data_complete_title)
gt_data_complete_title = gt_data_complete_title[order(match(gt_data_complete_title[, 1], gt_data_main_title$title)), ]
rownames(gt_data_complete_title) = 1:nrow(gt_data_complete_title)

rm(list = ls() [which(ls() != "gt_data_complete_title")])
save.image("data/gt_data_complete_title.RData")


# Concatenate Workspaces and save them  ===============================================================================
load("data/gt_data_ankers.RData")
gt_kino <- gtrends(keyword = "Kino", geo = "DE", 
                  time = paste(as.character(trunc(startdate, "months")),
                               as.character(ceil(enddate, "months"))), onlyInterest = TRUE)$interest_over_time
gt_kino$date <- as.character(gt_kino$date)
rm(list = ls() [which(ls() != "gt_kino")])
load("data/gt_data_main_title_film.RData")
load("data/gt_data_complete_title.RData")
load("data/gt_data_main_title.RData")

save.image("data/gt_data_complete.RData")
