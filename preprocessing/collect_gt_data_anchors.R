source("utils/data_gathering_utils.R")
load("data/movies_preprocessed.RData")

startdates <- movies$forecast_start
startdates <- trunc(x = startdates, units = "days")
enddate <- tail(last_forecast_date, 1)
films <- movies$title
startdates_median <- startdates - as.difftime(tim = 52, units = "weeks")
startdate <- startdates_median[1]
enddates_median <- enddate + as.difftime(tim = 9, units = "weeks")


# Define anker terms
ankers <- c("Hamburg", "Frankfurt", "Spiegel", "Braunschweig", "Siegen", "Plauen", "Bad Hersfeld", "Reinbek", 
           "Nordkurier", "Allgäuer Zeitung", "Fränkische Nachrichten", "Schleswiger Nachrichten")

# Link first anker to its median
gt_anker1_median <- gtrends(
  keyword = ankers[1], geo = "DE", 
  time = paste(as.character(trunc(startdate, "months")), as.character(ceil(enddate, "months"))))
gt_anker1_median <- gt_anker1_median$interest_over_time
gt_anker1_median <- gt_anker1_median$hits / median(gt_anker1_median$hits)

for (i in 2:length(ankers)){
  assign(paste0("gt_anker", i-1, "_anker", i),
         export_gt_data(terms = ankers[i], startdates = startdate, enddates = enddate, anker = ankers[i-1]))
}

# Scale down anker terms
gt_anker1_anker2 <- gt_anker1_median * gt_anker1_anker2[, 4]

for (i in 3:length(ankers)){
  assign(paste0("gt_anker", 1, "_anker", i), 
         get(paste0("gt_anker", 1, "_anker", i-1)) * get(paste0("gt_anker", i-1, "_anker", i))[, 4])
}

save.image("data/gt_data_ankers.RData")
