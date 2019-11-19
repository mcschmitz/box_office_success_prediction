source("utils/data_gathering_utils.R")
load("data/movies_preprocessed.RData")
save.image("data/gt-data_anker/gt-data_ankerwords.RData")

# Gather data for the main title of the movie ==========================================================================
# Gather data with Level 1 Anker "Hamburg"
gt_hamburg = exportGT(terms = main_title, startdates = startdates_median[1], enddates = enddates[898],
                      anker = "Hamburg")

colnames(gt_hamburg) [4:ncol(gt_hamburg)] = gsub(films, pattern = " ", replacement = "_")

# Detect low level movies
levels_hamburg = detect_low_level_terms(terms = main_title, trends = gt_hamburg, enddate = end_forecast_week)
main_title[levels_hamburg$high]

save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 2 Anker "Frankfurt"
terms_frankfurt = main_title[levels_hamburg$low]
gt_frankfurt = exportGT(terms = terms_frankfurt, startdates = startdates_median[1], enddates = enddates[898],
                              anker = "Frankfurt")
films_frankfurt = films[levels_hamburg$low]
colnames(gt_frankfurt) [4:ncol(gt_frankfurt)] = gsub(films_frankfurt, pattern = " ", replacement = "_")
end_forecast_week_frankfurt = end_forecast_week[levels_hamburg$low]
levels_frankfurt = detect_low_level_terms(terms = terms_frankfurt, trends = gt_frankfurt, 
                                          enddate = end_forecast_week_frankfurt)
terms_frankfurt[levels_frankfurt$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")


# Gather data with Level 3 Anker "Spiegel"
terms_spiegel = terms_frankfurt[levels_frankfurt$low]
gt_spiegel = exportGT(terms = terms_spiegel, startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Spiegel")
films_spiegel = films_frankfurt[levels_frankfurt$low]
colnames(gt_spiegel) [4:ncol(gt_spiegel)] = gsub(films_spiegel, pattern = " ", replacement = "_")
end_forecast_week_spiegel = end_forecast_week_frankfurt[levels_frankfurt$low]
levels_spiegel = detect_low_level_terms(terms = terms_spiegel, trends = gt_spiegel, enddate = end_forecast_week_spiegel)
terms_spiegel[levels_spiegel$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 4 Anker "Braunschweig"
terms_braunschweig = terms_spiegel[levels_spiegel$low]
gt_braunschweig = exportGT(terms = terms_braunschweig, startdates = startdates_median[1], 
                                 enddates = enddates[898], anker = "Braunschweig")
films_braunschweig = films_spiegel[levels_spiegel$low]
colnames(gt_braunschweig) [4:ncol(gt_braunschweig)] = gsub(films_braunschweig, pattern = " ", replacement = "_")
end_forecast_week_braunschweig = end_forecast_week_spiegel[levels_spiegel$low]
levels_braunschweig = detect_low_level_terms(terms = terms_braunschweig, trends = gt_braunschweig, 
                                             enddate = end_forecast_week_braunschweig)
terms_braunschweig[levels_braunschweig$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 5 Anker "Siegen"
terms_siegen = terms_braunschweig[levels_braunschweig$low]
gt_siegen = exportGT(terms = terms_siegen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Siegen")
films_siegen = films_braunschweig[levels_braunschweig$low]
colnames(gt_siegen)[4:ncol(gt_siegen)] = gsub(films_siegen, pattern = " ", replacement = "_")
end_forecast_week_siegen = end_forecast_week_braunschweig[levels_braunschweig$low]
levels_siegen = detect_low_level_terms(terms = terms_siegen, trends = gt_siegen, enddate = end_forecast_week_siegen)
terms_siegen[levels_siegen$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 6 Anker "Plauen"
terms_plauen = terms_siegen[levels_siegen$low]
gt_plauen = exportGT(terms = terms_plauen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Plauen")
films_plauen = films_siegen[levels_siegen$low]
colnames(gt_plauen)[4:ncol(gt_plauen)] = gsub(films_plauen, pattern = " ", replacement = "_")
end_forecast_week_plauen = end_forecast_week_siegen[levels_siegen$low]
levels_plauen = detect_low_level_terms(terms = terms_plauen, trends = gt_plauen, enddate = end_forecast_week_plauen)
terms_plauen[levels_plauen$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 7 Anker "Bad Hersfeld"
terms_badhersfeld = terms_plauen[levels_plauen$low]
gt_badhersfeld = exportGT(terms = terms_badhersfeld, startdates = startdates_median[1], enddates = enddates[898],
                                anker = "Bad Hersfeld")
films_badhersfeld = films_plauen[levels_plauen$low]
colnames(gt_badhersfeld)[4:ncol(gt_badhersfeld)] = gsub(films_badhersfeld, pattern = " ", replacement = "_")
end_forecast_week_badhersfeld = end_forecast_week_plauen[levels_plauen$low]
levels_badhersfeld = detect_low_level_terms(terms = terms_badhersfeld, trends = gt_badhersfeld,
                                            enddate = end_forecast_week_badhersfeld)
terms_badhersfeld[levels_badhersfeld$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 8 Anker "Reinbek"
terms_reinbek = terms_badhersfeld[levels_badhersfeld$low]
gt_reinbek = exportGT(terms = terms_reinbek, startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Reinbek")
films_reinbek = films_badhersfeld[levels_badhersfeld$low]
colnames(gt_reinbek)[4:ncol(gt_reinbek)] = gsub(films_reinbek, pattern = " ", replacement = "_")
end_forecast_week_reinbek = end_forecast_week_badhersfeld[levels_badhersfeld$low]
levels_reinbek = detect_low_level_terms(terms = terms_reinbek, trends = gt_reinbek, enddate = end_forecast_week_reinbek)
terms_reinbek[levels_reinbek$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 9 Anker "Nordkurier"
terms_nordkurier = terms_reinbek[levels_reinbek$low]
gt_nordkurier = exportGT(terms = terms_nordkurier, startdates = startdates_median[1], enddates = enddates[898],
                               anker = "Nordkurier")
films_nordkurier = films_reinbek[levels_reinbek$low]
colnames(gt_nordkurier)[4:ncol(gt_nordkurier)] = gsub(films_nordkurier, pattern = " ", replacement = "_")
end_forecast_week_nordkurier = end_forecast_week_reinbek[levels_reinbek$low]
levels_nordkurier = detect_low_level_terms(terms = terms_nordkurier, trends = gt_nordkurier, 
                                           enddate = end_forecast_week_nordkurier)
terms_nordkurier[levels_nordkurier$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 10 Anker "Allgaeuer Zeitung"
terms_allgaeuerzeitung = terms_nordkurier[levels_nordkurier$low]
gt_allgaeuerzeitung = exportGT(terms = terms_allgaeuerzeitung, startdates = startdates_median[1],
                                     enddates = enddates[898], anker = "Allgäuer Zeitung")
films_allgaeuerzeitung = films_nordkurier[levels_nordkurier$low]
colnames(gt_allgaeuerzeitung)[4:ncol(gt_allgaeuerzeitung)] =
  gsub(films_allgaeuerzeitung, pattern = " ", replacement = "_")
end_forecast_week_allgaeuerzeitung = end_forecast_week_nordkurier[levels_nordkurier$low]
levels_allgaeuerzeitung = detect_low_level_terms(terms = terms_allgaeuerzeitung, trends = gt_allgaeuerzeitung,
                                                 enddate = end_forecast_week_allgaeuerzeitung)
terms_allgaeuerzeitung[levels_allgaeuerzeitung$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 11 Anker "Fraenkische Nachrichten"
terms_fraenkischenachrichten = terms_allgaeuerzeitung[levels_allgaeuerzeitung$low]
gt_fraenkischenachrichten = exportGT(terms = terms_fraenkischenachrichten, startdates = startdates_median[1],
                                           enddates = enddates[898], anker = "Fränkische Nachrichten")
films_fraenkischenachrichten = films_allgaeuerzeitung[levels_allgaeuerzeitung$low]
colnames(gt_fraenkischenachrichten)[4:ncol(gt_fraenkischenachrichten)] = 
  gsub(films_fraenkischenachrichten, pattern = " ", replacement = "_")
end_forecast_week_fraenkischenachrichten = end_forecast_week_allgaeuerzeitung[levels_allgaeuerzeitung$low]
levels_fraenkischenachrichten = detect_low_level_terms(terms = terms_fraenkischenachrichten,
                                                       trends = gt_fraenkischenachrichten,
                                                       enddate = end_forecast_week_fraenkischenachrichten)
terms_fraenkischenachrichten[levels_fraenkischenachrichten$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 12 Anker "Schleswiger Nachrichten"
terms_schleswigernachrichten = terms_fraenkischenachrichten[levels_fraenkischenachrichten$low]
gt_schleswigernachrichten = exportGT(terms = terms_schleswigernachrichten, startdates = startdates_median[1],
                                           enddates = enddates[898], anker = "Schleswiger Nachrichten")
films_schleswigernachrichten = films_fraenkischenachrichten[levels_fraenkischenachrichten$low]
colnames(gt_schleswigernachrichten)[4:ncol(gt_schleswigernachrichten)] = 
  gsub(films_schleswigernachrichten, pattern = " ", replacement = "_")
# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_schleswigernachrichten = end_forecast_week_fraenkischenachrichten[levels_fraenkischenachrichten$low]
levels_schleswigernachrichten = detect_low_level_terms(terms = terms_schleswigernachrichten,
                                                       trends = gt_schleswigernachrichten,
                                                       enddate = end_forecast_week_schleswigernachrichten)
terms_schleswigernachrichten[levels_schleswigernachrichten$low]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Scale down search terms
load("data/gt-data_anker/gt-data_anker_main_title.RData")
load("data/gt-data_anker/gt-data_ankerwords.RData")

# Scale level "Hamburg" with respect to level "Hamburg Median":
gt_hamburg_scaled = scale_down(upper_gt = gt_hamburg, lower_gt = gt_hamburg, scale_gt = gt_hamburg_median)

# Scale Level "Frankfurt" with respect to Level "Hamburg":
gt_frankfurt_scaled = scale_down(upper_gt = gt_hamburg_scaled, lower_gt = gt_frankfurt, scale_gt = gt_hamburg_frankfurt)

gt_spiegel_scaled = scale_down(upper_gt = gt_frankfurt_scaled, lower_gt = gt_spiegel, scale_gt = gt_hamburg_spiegel)
gt_braunschweig_scaled = scale_down(upper_gt = gt_spiegel_scaled, lower_gt = gt_braunschweig,
                                    scale_gt = gt_hamburg_braunschweig)
gt_siegen_scaled = scale_down(upper_gt = gt_braunschweig_scaled, lower_gt = gt_siegen, 
                              scale_gt = gt_hamburg_siegen)
gt_plauen_scaled = scale_down(upper_gt = gt_siegen_scaled, lower_gt = gt_plauen, scale_gt = gt_hamburg_plauen)
gt_badhersfeld_scaled = scale_down(upper_gt = gt_plauen_scaled, lower_gt = gt_badhersfeld, 
                                   scale_gt = gt_hamburg_badhersfeld)
gt_reinbek_scaled = scale_down(upper_gt = gt_badhersfeld_scaled, lower_gt = gt_reinbek, scale_gt = gt_hamburg_reinbek)
gt_nordkurier_scaled = scale_down(upper_gt = gt_reinbek_scaled, lower_gt = gt_nordkurier, 
                                  scale_gt = gt_hamburg_nordkurier)
gt_allgaeuerzeitung_scaled = scale_down(upper_gt = gt_nordkurier_scaled, lower_gt = gt_allgaeuerzeitung,
                                        scale_gt = gt_hamburg_allgaeuerzeitung)
gt_fraenkischenachrichten_scaled = scale_down(upper_gt = gt_allgaeuerzeitung_scaled, 
                                              lower_gt = gt_fraenkischenachrichten,
                                              scale_gt = gt_hamburg_fraenkischenachrichten)
gt_schleswigernachrichten_scaled = scale_down(upper_gt = gt_fraenkischenachrichten_scaled,
                                              lower_gt = gt_schleswigernachrichten,
                                              scale_gt = gt_hamburg_schleswigernachrichten)

# Build final Google Trends dataset
gt_data_main_title = gt_schleswigernachrichten_scaled
gt_data_main_title = as.data.frame(t(gt_data_main_title[,4:ncol(gt_data_main_title)]))
colnames(gt_data_main_title) = gt_hamburg[, 2]
rownames(gt_data_main_title) = 1:nrow(gt_data_main_title)
gt_data_main_title = cbind(movies$Filmtitel, gt_data_main_title)
head(gt_data_main_title)
rm(list = ls() [which(ls() != "gt_data_main_title")])
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")


# Gather data for the main title of the movie with suffix "film" =======================================================
load("data/gt-data_anker/gt-data_ankerwords.RData")
gt_hamburg = exportGT(terms = main_title_film, startdates = startdates_median[1], enddates = enddates[898], 
                      anker = "Hamburg")
colnames(gt_hamburg) [4:ncol(gt_hamburg)] = gsub(films, pattern = " ", replacement = "_")
levels_hamburg = detect_low_level_terms(terms = main_title_film, trends = gt_hamburg, enddate = end_forecast_week)
main_title_film[levels_hamburg$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_frankfurt = main_title_film[levels_hamburg$low]
gt_frankfurt = exportGT(terms = terms_frankfurt, startdates = startdates_median[1], enddates = enddates[898],
                              anker = "Frankfurt")
films_frankfurt = films[levels_hamburg$low]
colnames(gt_frankfurt) [4:ncol(gt_frankfurt)] = gsub(films_frankfurt, pattern = " ", replacement = "_")
end_forecast_week_frankfurt = end_forecast_week[levels_hamburg$low]
levels_frankfurt = detect_low_level_terms(terms = terms_frankfurt, trends = gt_frankfurt, 
                                          enddate = end_forecast_week_frankfurt)
terms_frankfurt[levels_frankfurt$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_spiegel = terms_frankfurt[levels_frankfurt$low]
gt_spiegel = exportGT(terms = terms_spiegel, startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Spiegel")
films_spiegel = films_frankfurt[levels_frankfurt$low]
colnames(gt_spiegel) [4:ncol(gt_spiegel)] = gsub(films_spiegel, pattern = " ", replacement = "_")
end_forecast_week_spiegel = end_forecast_week_frankfurt[levels_frankfurt$low]
levels_spiegel = detect_low_level_terms(terms = terms_spiegel, trends = gt_spiegel, enddate = end_forecast_week_spiegel)
terms_spiegel[levels_spiegel$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_braunschweig = terms_spiegel[levels_spiegel$low]
gt_braunschweig = exportGT(terms = terms_braunschweig, startdates = startdates_median[1],
                                 enddates = enddates[898], anker = "Braunschweig")
films_braunschweig = films_spiegel[levels_spiegel$low]
colnames(gt_braunschweig) [4:ncol(gt_braunschweig)] = gsub(films_braunschweig, pattern = " ", replacement = "_")
end_forecast_week_braunschweig = end_forecast_week_spiegel[levels_spiegel$low]
levels_braunschweig = detect_low_level_terms(terms = terms_braunschweig, trends = gt_braunschweig, 
                                             enddate = end_forecast_week_braunschweig)
terms_braunschweig[levels_braunschweig$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_siegen = terms_braunschweig[levels_braunschweig$low]
gt_siegen = exportGT(terms = terms_siegen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Siegen")
films_siegen = films_braunschweig[levels_braunschweig$low]
colnames(gt_siegen)[4:ncol(gt_siegen)] = gsub(films_siegen, pattern = " ", replacement = "_")
end_forecast_week_siegen = end_forecast_week_braunschweig[levels_braunschweig$low]
levels_siegen = detect_low_level_terms(terms = terms_siegen, trends = gt_siegen, enddate = end_forecast_week_siegen)
terms_siegen[levels_siegen$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_plauen = terms_siegen[levels_siegen$low]
gt_plauen = exportGT(terms = terms_plauen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Plauen")
films_plauen = films_siegen[levels_siegen$low]
colnames(gt_plauen)[4:ncol(gt_plauen)] =gsub(films_plauen, pattern = " ", replacement = "_")
end_forecast_week_plauen = end_forecast_week_siegen[levels_siegen$low]
levels_plauen = detect_low_level_terms(terms = terms_plauen, trends = gt_plauen, enddate = end_forecast_week_plauen)
terms_plauen[levels_plauen$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

terms_badhersfeld = terms_plauen[levels_plauen$low]
gt_badhersfeld = exportGT(terms = terms_badhersfeld, startdates = startdates[1], enddates = enddates[898],
                                anker = "Bad Hersfeld")
films_badhersfeld = films_plauen[levels_plauen$low]
colnames(gt_badhersfeld)[4:ncol(gt_badhersfeld)] = gsub(films_badhersfeld, pattern = " ", replacement = "_")
end_forecast_week_badhersfeld = end_forecast_week_plauen[levels_plauen$low]
levels_badhersfeld = detect_low_level_terms(terms = terms_badhersfeld, trends = gt_badhersfeld,
                                            enddate = end_forecast_week_badhersfeld)
terms_badhersfeld[levels_badhersfeld$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_reinbek = terms_badhersfeld[levels_badhersfeld$low]
gt_reinbek = exportGT(terms = terms_reinbek, startdates = startdates[1], enddates = enddates[898],
                            anker = "Reinbek")
films_reinbek = films_badhersfeld[levels_badhersfeld$low]
colnames(gt_reinbek)[4:ncol(gt_reinbek)] = gsub(films_reinbek, pattern = " ", replacement = "_")
end_forecast_week_reinbek = end_forecast_week_badhersfeld[levels_badhersfeld$low]
levels_reinbek = detect_low_level_terms(terms = terms_reinbek, trends = gt_reinbek, enddate = end_forecast_week_reinbek)
terms_reinbek[levels_reinbek$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_nordkurier = terms_reinbek[levels_reinbek$low]
gt_nordkurier = exportGT(terms = terms_nordkurier, startdates = startdates[1], enddates = enddates[898], 
                               anker = "Nordkurier")
films_nordkurier = films_reinbek[levels_reinbek$low]
colnames(gt_nordkurier)[4:ncol(gt_nordkurier)] = gsub(films_nordkurier, pattern = " ", replacement = "_")
end_forecast_week_nordkurier = end_forecast_week_reinbek[levels_reinbek$low]
levels_nordkurier = detect_low_level_terms(terms = terms_nordkurier, trends = gt_nordkurier,
                                           enddate = end_forecast_week_nordkurier)
terms_nordkurier[levels_nordkurier$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_allgaeuerzeitung = terms_nordkurier[levels_nordkurier$low]
gt_allgaeuerzeitung = exportGT(terms = terms_allgaeuerzeitung, startdates = startdates[1], 
                                     enddates = enddates[898], anker = "Allgäuer Zeitung")
films_allgaeuerzeitung = films_nordkurier[levels_nordkurier$low]
colnames(gt_allgaeuerzeitung)[4:ncol(gt_allgaeuerzeitung)] =
  gsub(films_allgaeuerzeitung, pattern = " ", replacement = "_")
end_forecast_week_allgaeuerzeitung = end_forecast_week_nordkurier[levels_nordkurier$low]
levels_allgaeuerzeitung = detect_low_level_terms(terms = terms_allgaeuerzeitung, trends = gt_allgaeuerzeitung,
                                                 enddate = end_forecast_week_allgaeuerzeitung)
terms_allgaeuerzeitung[levels_allgaeuerzeitung$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_fraenkischenachrichten = terms_allgaeuerzeitung[levels_allgaeuerzeitung$low]
gt_fraenkischenachrichten = exportGT(terms = terms_fraenkischenachrichten, startdates = startdates[1],
                                           enddates = enddates[898], anker = "Fränkische Nachrichten")
films_fraenkischenachrichten = films_allgaeuerzeitung[levels_allgaeuerzeitung$low]
colnames(gt_fraenkischenachrichten)[4:ncol(gt_fraenkischenachrichten)] = 
  gsub(films_fraenkischenachrichten, pattern = " ", replacement = "_")
end_forecast_week_fraenkischenachrichten = end_forecast_week_allgaeuerzeitung[levels_allgaeuerzeitung$low]
levels_fraenkischenachrichten = detect_low_level_terms(terms = terms_fraenkischenachrichten,
                                                       trends = gt_fraenkischenachrichten,
                                                       enddate = end_forecast_week_fraenkischenachrichten)
terms_fraenkischenachrichten[levels_fraenkischenachrichten$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

terms_schleswigernachrichten = terms_fraenkischenachrichten[levels_fraenkischenachrichten$low]
gt_schleswigernachrichten = exportGT(terms = terms_schleswigernachrichten, startdates = startdates[1],
                                           enddates = enddates[898], anker = "Schleswiger Nachrichten")
films_schleswigernachrichten = films_fraenkischenachrichten[levels_fraenkischenachrichten$low]
colnames(gt_schleswigernachrichten)[4:ncol(gt_schleswigernachrichten)] = 
  gsub(films_schleswigernachrichten, pattern = " ", replacement = "_")
end_forecast_week_schleswigernachrichten = end_forecast_week_fraenkischenachrichten[levels_fraenkischenachrichten$low]
levels_schleswigernachrichten = detect_low_level_terms(terms = terms_schleswigernachrichten, 
                                                       trends = gt_schleswigernachrichten,
                                                       enddate = end_forecast_week_schleswigernachrichten)
terms_schleswigernachrichten[levels_schleswigernachrichten$high]
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")

load("data/gt-data_anker/gt-data_ankerwords.RData")
load("data/gt-data_anker/gt-data_anker_main_title_film.RData")

gt_hamburg_scaled = scale_down(upper_gt = gt_hamburg, lower_gt = gt_hamburg, scale_gt = gt_hamburg_median)
gt_frankfurt_scaled = scale_down(upper_gt = gt_hamburg, lower_gt = gt_frankfurt, scale_gt = gt_hamburg_frankfurt)
gt_spiegel_scaled = scale_down(upper_gt = gt_frankfurt_scaled, lower_gt = gt_spiegel, scale_gt = gt_hamburg_spiegel)
gt_braunschweig_scaled = scale_down(upper_gt = gt_spiegel_scaled, lower_gt = gt_braunschweig, 
                                    scale_gt = gt_hamburg_braunschweig)
gt_siegen_scaled = scale_down(upper_gt = gt_braunschweig_scaled, lower_gt = gt_siegen, scale_gt = gt_hamburg_siegen)
gt_plauen_scaled = scale_down(upper_gt = gt_siegen_scaled, lower_gt = gt_plauen, scale_gt = gt_hamburg_plauen)
gt_badhersfeld_scaled = scale_down(upper_gt = gt_plauen_scaled, lower_gt = gt_badhersfeld, 
                                   scale_gt = gt_hamburg_badhersfeld)
gt_reinbek_scaled = scale_down(upper_gt = gt_badhersfeld_scaled, lower_gt = gt_reinbek, scale_gt = gt_hamburg_reinbek)
gt_nordkurier_scaled = scale_down(upper_gt = gt_reinbek_scaled, lower_gt = gt_nordkurier, 
                                  scale_gt = gt_hamburg_nordkurier)
gt_allgaeuerzeitung_scaled = scale_down(upper_gt = gt_nordkurier_scaled, lower_gt = gt_allgaeuerzeitung,
                                        scale_gt = gt_hamburg_allgaeuerzeitung)
gt_fraenkischenachrichten_scaled = scale_down(upper_gt = gt_allgaeuerzeitung_scaled, 
                                              lower_gt = gt_fraenkischenachrichten,
                                              scale_gt = gt_hamburg_fraenkischenachrichten)
gt_schleswigernachrichten_scaled = scale_down(upper_gt = gt_fraenkischenachrichten_scaled,
                                              lower_gt = gt_schleswigernachrichten,
                                              scale_gt = gt_hamburg_schleswigernachrichten)
gt_data_main_title_film = gt_schleswigernachrichten_scaled
gt_data_main_title_film = as.data.frame(t(gt_data_main_title_film[,4:ncol(gt_data_main_title_film)]))
colnames(gt_data_main_title_film) = gt_hamburg[, 2]
rownames(gt_data_main_title_film) = 1:nrow(gt_data_main_title)
gt_data_main_title_film = cbind(movies$Filmtitel, gt_data_main_title_film)
head(gt_data_main_title_film)
rm(list = ls() [which(ls() != "gt_data_main_title_film")])
save.image("data/gt-data_anker/gt-data_anker_main_title_film.RData")


# Gather data for the complete title of the movie ======================================================================
load("data/gt-data_anker/gt-data_ankerwords.RData")
# Filter for movies which main title differs from the complete title
movies_complete_title = complete_title[which(main_title != complete_title)]
end_forecast_week = end_forecast_week[which(main_title != complete_title)]

gt_hamburg = exportGT(terms = movies_complete_title, startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Hamburg")
films_hamburg = films[main_title != complete_title]
colnames(gt_hamburg) [4:ncol(gt_hamburg)] =  gsub(films_hamburg, pattern = " ", replacement = "_")
levels_hamburg = detect_low_level_terms(terms = movies_complete_title, trends = gt_hamburg, enddate = end_forecast_week)
movies_complete_title[levels_hamburg$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_frankfurt = movies_complete_title[levels_hamburg$low]
gt_frankfurt = exportGT(terms = terms_frankfurt, startdates = startdates_median[1], enddates = enddates[898],
                              anker = "Frankfurt")
films_frankfurt = films_hamburg[levels_hamburg$low]
colnames(gt_frankfurt) [4:ncol(gt_frankfurt)] = gsub(films_frankfurt, pattern = " ", replacement = "_")
end_forecast_week_frankfurt = end_forecast_week[levels_hamburg$low]
levels_frankfurt = detect_low_level_terms(terms = terms_frankfurt, trends = gt_frankfurt, 
                                          enddate = end_forecast_week_frankfurt)
terms_frankfurt[levels_frankfurt$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_spiegel = terms_frankfurt[levels_frankfurt$low]
gt_spiegel = exportGT(terms = terms_spiegel,startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Spiegel")
films_spiegel = films_frankfurt[levels_frankfurt$low]
colnames(gt_spiegel) [4:ncol(gt_spiegel)] = gsub(films_spiegel, pattern = " ", replacement = "_")
end_forecast_week_spiegel = end_forecast_week_frankfurt[levels_frankfurt$low]
levels_spiegel = detect_low_level_terms(terms = terms_spiegel, trends = gt_spiegel, enddate = end_forecast_week_spiegel)
terms_spiegel[levels_spiegel$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_braunschweig = terms_spiegel[levels_spiegel$low]
gt_braunschweig = exportGT(terms = terms_braunschweig, startdates = startdates_median[1], 
                                 enddates = enddates[898], anker = "Braunschweig")
films_braunschweig = films_spiegel[levels_spiegel$low]
colnames(gt_braunschweig) [4:ncol(gt_braunschweig)] = gsub(films_braunschweig, pattern = " ", replacement = "_")
end_forecast_week_braunschweig = end_forecast_week_spiegel[levels_spiegel$low]
levels_braunschweig = detect_low_level_terms(terms = terms_braunschweig, trends = gt_braunschweig,
                                             enddate = end_forecast_week_braunschweig)
terms_braunschweig[levels_braunschweig$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_siegen = terms_braunschweig[levels_braunschweig$low]
gt_siegen = exportGT(terms = terms_siegen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Siegen")
films_siegen = films_braunschweig[levels_braunschweig$low]
colnames(gt_siegen)[4:ncol(gt_siegen)] = gsub(films_siegen, pattern = " ", replacement = "_")
end_forecast_week_siegen = end_forecast_week_braunschweig[levels_braunschweig$low]
levels_siegen = detect_low_level_terms(terms = terms_siegen, trends = gt_siegen, enddate = end_forecast_week_siegen)
terms_siegen[levels_siegen$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_plauen = terms_siegen[levels_siegen$low]
gt_plauen = exportGT(terms = terms_plauen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Plauen")
films_plauen = films_siegen[levels_siegen$low]
colnames(gt_plauen)[4:ncol(gt_plauen)] = gsub(films_plauen, pattern = " ", replacement = "_")
end_forecast_week_plauen = end_forecast_week_siegen[levels_siegen$low]
levels_plauen = detect_low_level_terms(terms = terms_plauen, trends = gt_plauen, enddate = end_forecast_week_plauen)
terms_plauen[levels_plauen$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_badhersfeld = terms_plauen[levels_plauen$low]
gt_badhersfeld = exportGT(terms = terms_badhersfeld, startdates = startdates_median[1],
                                enddates = enddates[898], anker = "Bad Hersfeld")
films_badhersfeld = films_plauen[levels_plauen$low]
colnames(gt_badhersfeld)[4:ncol(gt_badhersfeld)] = gsub(films_badhersfeld, pattern = " ", replacement = "_")
end_forecast_week_badhersfeld = end_forecast_week_plauen[levels_plauen$low]
levels_badhersfeld = detect_low_level_terms(terms = terms_badhersfeld, trends = gt_badhersfeld,
                                            enddate = end_forecast_week_badhersfeld)
terms_badhersfeld[levels_badhersfeld$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_reinbek = terms_badhersfeld[levels_badhersfeld$low]
gt_reinbek = exportGT(terms = terms_reinbek, startdates = startdates_median[1],
                            enddates = enddates[898], anker = "Reinbek")
films_reinbek = films_badhersfeld[levels_badhersfeld$low]
colnames(gt_reinbek)[4:ncol(gt_reinbek)] = gsub(films_reinbek, pattern = " ", replacement = "_")
end_forecast_week_reinbek = end_forecast_week_badhersfeld[levels_badhersfeld$low]
levels_reinbek = detect_low_level_terms(terms = terms_reinbek, trends = gt_reinbek, enddate = end_forecast_week_reinbek)
terms_reinbek[levels_reinbek$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_nordkurier = terms_reinbek[levels_reinbek$low]
gt_nordkurier = exportGT(terms = terms_nordkurier, startdates = startdates_median[1], enddates = enddates[898],
                               anker = "Nordkurier")
films_nordkurier = films_reinbek[levels_reinbek$low]
colnames(gt_nordkurier)[4:ncol(gt_nordkurier)] = gsub(films_nordkurier, pattern = " ", replacement = "_")
end_forecast_week_nordkurier = end_forecast_week_reinbek[levels_reinbek$low]
levels_nordkurier = detect_low_level_terms(terms = terms_nordkurier, trends = gt_nordkurier,
                                           enddate = end_forecast_week_nordkurier)
terms_nordkurier[levels_nordkurier$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_allgaeuerzeitung = terms_nordkurier[levels_nordkurier$low]
gt_allgaeuerzeitung = exportGT(terms = terms_allgaeuerzeitung, startdates = startdates_median[1],
                                     enddates = enddates[898], anker = "Allgäuer Zeitung")
films_allgaeuerzeitung = films_nordkurier[levels_nordkurier$low]
colnames(gt_allgaeuerzeitung)[4:ncol(gt_allgaeuerzeitung)] = 
  gsub(films_allgaeuerzeitung, pattern = " ", replacement = "_")
end_forecast_week_allgaeuerzeitung = end_forecast_week_nordkurier[levels_nordkurier$low]
levels_allgaeuerzeitung = detect_low_level_terms(terms = terms_allgaeuerzeitung, trends = gt_allgaeuerzeitung,
                                                 enddate = end_forecast_week_allgaeuerzeitung)
terms_allgaeuerzeitung[levels_allgaeuerzeitung$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_fraenkischenachrichten = terms_allgaeuerzeitung[levels_allgaeuerzeitung$low]
gt_fraenkischenachrichten = exportGT(terms = terms_fraenkischenachrichten, startdates = startdates_median[1],
                                           enddates = enddates[898], anker = "Fränkische Nachrichten")
films_fraenkischenachrichten = films_allgaeuerzeitung[levels_allgaeuerzeitung$low]
colnames(gt_fraenkischenachrichten)[4:ncol(gt_fraenkischenachrichten)] = 
  gsub(films_fraenkischenachrichten, pattern = " ", replacement = "_")
end_forecast_week_fraenkischenachrichten = end_forecast_week_allgaeuerzeitung[levels_allgaeuerzeitung$low]
levels_fraenkischenachrichten = detect_low_level_terms(terms = terms_fraenkischenachrichten,
                                                       trends = gt_fraenkischenachrichten,
                                                       enddate = end_forecast_week_fraenkischenachrichten)
terms_fraenkischenachrichten[levels_fraenkischenachrichten$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

terms_schleswigernachrichten = terms_fraenkischenachrichten[levels_fraenkischenachrichten$low]
gt_schleswigernachrichten = exportGT(terms = terms_schleswigernachrichten, startdates = startdates_median[1],
                                           enddates = enddates[898], anker = "Schleswiger Nachrichten")
films_schleswigernachrichten = films_fraenkischenachrichten[levels_fraenkischenachrichten$low]
colnames(gt_schleswigernachrichten)[4:ncol(gt_schleswigernachrichten)] = 
  gsub(films_schleswigernachrichten, pattern = " ", replacement = "_")
end_forecast_week_schleswigernachrichten = end_forecast_week_fraenkischenachrichten[levels_fraenkischenachrichten$low]
levels_schleswigernachrichten = detect_low_level_terms(terms = terms_schleswigernachrichten,
                                                       trends = gt_schleswigernachrichten,
                                                       enddate = end_forecast_week_schleswigernachrichten)
terms_schleswigernachrichten[levels_schleswigernachrichten$high]
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")

load("data/gt-data_anker/gt-data_anker_complete_title.RData")

gt_hamburg_scaled = scale_down(upper_gt = gt_hamburg, lower_gt = gt_hamburg, scale_gt = gt_hamburg_median)
gt_frankfurt_scaled = scale_down(upper_gt = gt_hamburg, lower_gt = gt_frankfurt, scale_gt = gt_hamburg_frankfurt)
gt_spiegel_scaled = scale_down(upper_gt = gt_frankfurt_scaled, lower_gt = gt_spiegel, scale_gt = gt_hamburg_spiegel)
gt_braunschweig_scaled = scale_down(upper_gt = gt_spiegel_scaled, lower_gt = gt_braunschweig, 
                                    scale_gt = gt_hamburg_braunschweig)
gt_siegen_scaled = scale_down(upper_gt = gt_braunschweig_scaled, lower_gt = gt_siegen, scale_gt = gt_hamburg_siegen)
gt_plauen_scaled = scale_down(upper_gt = gt_siegen_scaled, lower_gt = gt_plauen, scale_gt = gt_hamburg_plauen)
gt_badhersfeld_scaled = scale_down(upper_gt = gt_plauen_scaled, lower_gt = gt_badhersfeld, 
                                   scale_gt = gt_hamburg_badhersfeld)
gt_reinbek_scaled = scale_down(upper_gt = gt_badhersfeld_scaled, lower_gt = gt_reinbek, scale_gt = gt_hamburg_reinbek)
gt_nordkurier_scaled = scale_down(upper_gt = gt_reinbek_scaled, lower_gt = gt_nordkurier, 
                                  scale_gt = gt_hamburg_nordkurier)
gt_allgaeuerzeitung_scaled = scale_down(upper_gt = gt_nordkurier_scaled, lower_gt = gt_allgaeuerzeitung,
                                        scale_gt = gt_hamburg_allgaeuerzeitung)
gt_fraenkischenachrichten_scaled = scale_down(upper_gt = gt_allgaeuerzeitung_scaled, 
                                              lower_gt = gt_fraenkischenachrichten,
                                              scale_gt = gt_hamburg_fraenkischenachrichten)
gt_schleswigernachrichten_scaled = scale_down(upper_gt = gt_fraenkischenachrichten_scaled,
                                              lower_gt = gt_schleswigernachrichten,
                                              scale_gt = gt_hamburg_schleswigernachrichten)

gt_data_complete_title = gt_schleswigernachrichten_scaled
gt_data_complete_title = as.data.frame(t(gt_data_complete_title[,4:ncol(gt_data_complete_title)]))
rownames(gt_data_complete_title) = 1:nrow(gt_data_complete_title)
gt_data_complete_title = cbind(movies$Filmtitel[which(complete_title != main_title)], gt_data_complete_title)
rownames(gt_data_complete_title) = 1:nrow(gt_data_complete_title)
load("data/gt-data_anker/gt-data_anker_main_title.RData")
colnames(gt_data_complete_title) = colnames(gt_data_main_title)
gt_data_complete_title = rbind(gt_data_main_title[which(complete_title == main_title), ],
                                gt_data_complete_title)
gt_data_complete_title = gt_data_complete_title[order(match(gt_data_complete_title[, 1], movies$Filmtitel)), ]
rownames(gt_data_complete_title) = 1:nrow(gt_data_complete_title)
rm(list = ls() [which(ls() != "gt_data_complete_title")])
save.image("data/gt-data_anker/gt-data_anker_complete_title.RData")


# Concatenate Workspaces and save them
load("data/gt-data_anker/gt-data_anker_main_title.RData")
load("data/gt-data_anker/gt-data_anker_main_title_film.RData")
load("data/gt-data_anker/gt-data_anker_complete_title.RData")
gt_kino = gtrends(keyword = "Kino", geo = "DE", 
                  time = paste(as.character(trunc(startdates_median[1], "months")),
                               as.character(ceil(enddates[898], "months"))), onlyInterest = TRUE)$interest_over_time
gt_kino$date = as.character(gt_kino$date)
save.image("data/gt-data_anker/gt-data_anker_final.RData")
