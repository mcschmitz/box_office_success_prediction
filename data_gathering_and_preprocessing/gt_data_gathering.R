# Zugriff auf benötigte Programme und Workspaces:
source("utils/data_gathering_utils.R")
load("data/movies_preprocessed.RData")

startdates = movies$Prognose_start
startdates = trunc(x = startdates, units = "days")
enddates = end_forecast_week
films = movies$Filmtitel
startdates_median = startdates - as.difftime(tim = 52, units = "weeks")
enddates_median = enddates + as.difftime(tim = 9, units = "weeks")

# Link anker term "Hamburg" and its median
gt_hamburg_median = gtrends(keyword = "Hamburg", geo = "DE", 
                            time = paste(as.character(startdates_median[1]), as.character(enddates_median[898])))
gt_hamburg_median = gt_hamburg_median$interest_over_time
gt_hamburg_median_abs = median(gt_hamburg_median$hits)
gt_hamburg_median = gt_hamburg_median$hits / median(gt_hamburg_median$hits)

# Link "Hamburg" and "Frankfurt":
gt_hamburg_frankfurt = exportGT(terms = "Frankfurt", startdates = startdates_median[1], enddates = enddates[898], 
                                anker = "Hamburg")

# Link "Frankfurt" and "Spiegel":
gt_frankfurt_spiegel = exportGT(terms = "Spiegel", startdates = startdates_median[1], enddates = enddates[898],
                                anker = "Frankfurt")

gt_spiegel_braunschweig = exportGT(terms = "Braunschweig", startdates = startdates_median[1], enddates = enddates[898],
                                   anker = "Spiegel")

gt_braunschweig_siegen = exportGT(terms = "Siegen", startdates = startdates_median[1], enddates = enddates[898],
                                  anker = "Braunschweig")

gt_siegen_plauen = exportGT(terms = "Plauen", startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Siegen")

gt_plauen_badhersfeld = exportGT(terms = "Bad Hersfeld", startdates = startdates_median[1], enddates = enddates[898],
                                 anker = "Plauen")

gt_badhersfeld_reinbek = exportGT(terms = "Reinbek", startdates = startdates_median[1], enddates = enddates[898],
                                  anker = "Bad Hersfeld")

gt_reinbek_nordkurier = exportGT(terms = "Nordkurier", startdates = startdates_median[1], enddates = enddates[898],
                                 anker = "Reinbek")

gt_nordkurier_allgaeuerzeitung = exportGT(terms = "Allgäuer Zeitung", startdates = startdates_median[1], 
                                          enddates = enddates[898], anker = "Nordkurier")

gt_allgaeuerzeitung_fraenkischenachrichten = exportGT(terms = "Fränkische Nachrichten", 
                                                      startdates = startdates_median[1],  enddates = enddates[898],
                                                      anker = "Allgäuer Zeitung")

gt_fraenkischenachrichten_schleswigernachrichten = exportGT(terms = "Schleswiger Nachrichten", 
                                                            startdates = startdates_median[1], enddates = enddates[898],
                                                            anker = "Fränkische Nachrichten")

# Scale down anker terms
gt_hamburg_frankfurt = gt_hamburg_median[53:252] * gt_hamburg_frankfurt[, 4]
gt_hamburg_spiegel = gt_hamburg_frankfurt * gt_frankfurt_spiegel[, 4]
gt_hamburg_braunschweig = gt_hamburg_spiegel * gt_spiegel_braunschweig[, 4]
gt_hamburg_siegen = gt_hamburg_braunschweig * gt_braunschweig_siegen[, 4]
gt_hamburg_plauen = gt_hamburg_siegen * gt_siegen_plauen[, 4]
gt_hamburg_badhersfeld = gt_hamburg_plauen * gt_plauen_badhersfeld[, 4]
gt_hamburg_reinbek = gt_hamburg_badhersfeld * gt_badhersfeld_reinbek[, 4]
gt_hamburg_nordkurier = gt_hamburg_reinbek * gt_reinbek_nordkurier[, 4]
gt_hamburg_allgaeuerzeitung = gt_hamburg_nordkurier * gt_nordkurier_allgaeuerzeitung[, 4]
gt_hamburg_fraenkischenachrichten = gt_hamburg_allgaeuerzeitung * gt_allgaeuerzeitung_fraenkischenachrichten[, 4]
gt_hamburg_schleswigernachrichten = gt_hamburg_fraenkischenachrichten *
  gt_fraenkischenachrichten_schleswigernachrichten[, 4]

save.image("data/gt-data_anker/gt-data_ankerwords.RData")

# Gather data for the main title of the movie
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
gt_frankfurt = exportGT_anker(terms = terms_frankfurt, startdates = startdates_median[1], enddates = enddates[898],
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
gt_spiegel = exportGT_anker(terms = terms_spiegel, startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Spiegel")
films_spiegel = films_frankfurt[levels_frankfurt$low]
colnames(gt_spiegel) [4:ncol(gt_spiegel)] = gsub(films_spiegel, pattern = " ", replacement = "_")
end_forecast_week_spiegel = end_forecast_week_frankfurt[levels_frankfurt$low]
levels_spiegel = detect_low_level_terms(terms = terms_spiegel, trends = gt_spiegel, enddate = end_forecast_week_spiegel)
terms_spiegel[levels_spiegel$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 4 Anker "Braunschweig"
terms_braunschweig = terms_spiegel[levels_spiegel$low]
gt_braunschweig = exportGT_anker(terms = terms_braunschweig, startdates = startdates_median[1], 
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
gt_siegen = exportGT_anker(terms = terms_siegen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Siegen")
films_siegen = films_braunschweig[levels_braunschweig$low]
colnames(gt_siegen)[4:ncol(gt_siegen)] = gsub(films_siegen, pattern = " ", replacement = "_")
end_forecast_week_siegen = end_forecast_week_braunschweig[levels_braunschweig$low]
levels_siegen = detect_low_level_terms(terms = terms_siegen, trends = gt_siegen, enddate = end_forecast_week_siegen)
terms_siegen[levels_siegen$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 6 Anker "Plauen"
terms_plauen = terms_siegen[levels_siegen$low]
gt_plauen = exportGT_anker(terms = terms_plauen, startdates = startdates_median[1], enddates = enddates[898],
                           anker = "Plauen")
films_plauen = films_siegen[levels_siegen$low]
colnames(gt_plauen)[4:ncol(gt_plauen)] = gsub(films_plauen, pattern = " ", replacement = "_")
end_forecast_week_plauen = end_forecast_week_siegen[levels_siegen$low]
levels_plauen = detect_low_level_terms(terms = terms_plauen, trends = gt_plauen, enddate = end_forecast_week_plauen)
terms_plauen[levels_plauen$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 7 Anker "Bad Hersfeld"
terms_badhersfeld = terms_plauen[levels_plauen$low]
gt_badhersfeld = exportGT_anker(terms = terms_badhersfeld, startdates = startdates_median[1], enddates = enddates[898],
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
gt_reinbek = exportGT_anker(terms = terms_reinbek, startdates = startdates_median[1], enddates = enddates[898],
                            anker = "Reinbek")
films_reinbek = films_badhersfeld[levels_badhersfeld$low]
colnames(gt_reinbek)[4:ncol(gt_reinbek)] = gsub(films_reinbek, pattern = " ", replacement = "_")
end_forecast_week_reinbek = end_forecast_week_badhersfeld[levels_badhersfeld$low]
levels_reinbek = detect_low_level_terms(terms = terms_reinbek, trends = gt_reinbek, enddate = end_forecast_week_reinbek)
terms_reinbek[levels_reinbek$high]
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")

# Gather data with Level 9 Anker "Nordkurier"
terms_nordkurier = terms_reinbek[levels_reinbek$low]
gt_nordkurier = exportGT_anker(terms = terms_nordkurier, startdates = startdates_median[1], enddates = enddates[898],
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
gt_allgaeuerzeitung = exportGT_anker(terms = terms_allgaeuerzeitung, startdates = startdates_median[1],
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
gt_fraenkischenachrichten = exportGT_anker(terms = terms_fraenkischenachrichten, startdates = startdates_median[1],
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
gt_schleswigernachrichten = exportGT_anker(terms = terms_schleswigernachrichten, startdates = startdates_median[1],
                                           enddates = enddates[898], anker = "Schleswiger Nachrichten")
films_schleswigernachrichten = films_fraenkischenachrichten[levels_fraenkischenachrichten$low]
colnames(gt_schleswigernachrichten)[4:ncol(gt_schleswigernachrichten)] = 
  gsub(films_schleswigernachrichten, pattern = " ", replacement = "_")
# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_schleswigernachrichten = end_forecast_week_fraenkischenachrichten[levels_fraenkischenachrichten$low]
levels_schleswigernachrichten = detect_low_level_terms(terms = terms_schleswigernachrichten,
                                                       trends = gt_schleswigernachrichten,
                                                       enddate = end_forecast_week_schleswigernachrichten,
                                                       cutoff = 0.25)
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


# Main Title Film: ==================================================================

# Ziehung von Daten für Ankerwort Hamburg - Level 1 ============================

# Download der Daten:
gt_hamburg = exportGT(terms = main_title_film, startdates = startdates_median[1],
                            enddates = enddates[898],
                            user = "sl.consulting.ws@gmail.com",
                            pw = "consulting2016", anker = "Hamburg")

# Hinzufügen von Variablennamen zum Datensatz:
colnames(gt_hamburg) [4:ncol(gt_hamburg)] = 
  gsub(films, pattern = " ", replacement = "_")

# Auswahl der Filme, die beim naechsten Level nochmals gezogen werden müssen:
levels_hamburg = detect_low_level_terms(terms = main_title_film,
                                        trends = gt_hamburg,
                                        enddate = end_forecast_week)
main_title_film[levels_hamburg$high]
# Kein Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Frankfurt - Level 2 ==========================
# Download der Daten:
terms_frankfurt = main_title_film[levels_hamburg$low]
gt_frankfurt = exportGT_anker(terms = terms_frankfurt,
                              startdates = startdates_median[1],
                              enddates = enddates[898],
                              user = "sl.consulting.ws@gmail.com",
                              pw = "consulting2016", anker = "Frankfurt")

# Hinzufügen von Variablennamen zum Datensatz:
films_frankfurt = films[levels_hamburg$low]
colnames(gt_frankfurt) [4:ncol(gt_frankfurt)] = 
  gsub(films_frankfurt, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_frankfurt = end_forecast_week[levels_hamburg$low]
levels_frankfurt = detect_low_level_terms(terms = terms_frankfurt,
                                          trends = gt_frankfurt,
                                          enddate = end_forecast_week_frankfurt)
terms_frankfurt[levels_frankfurt$high]
# Kein Filme wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Spiegel - Level 3 ============================
# Download der Daten:
terms_spiegel = terms_frankfurt[levels_frankfurt$low]
gt_spiegel = exportGT_anker(terms = terms_spiegel, startdates = startdates_median[1],
                            enddates = enddates[898],
                            user = "sl.consulting.ws@gmail.com",
                            pw = "consulting2016", anker = "Spiegel")

# Hinzufügen von Variablennamen zum Datensatz:
films_spiegel = films_frankfurt[levels_frankfurt$low]
colnames(gt_spiegel) [4:ncol(gt_spiegel)] = 
  gsub(films_spiegel, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_spiegel = end_forecast_week_frankfurt[levels_frankfurt$low]
levels_spiegel = detect_low_level_terms(terms = terms_spiegel,
                                        trends = gt_spiegel,
                                        enddate = end_forecast_week_spiegel)
terms_spiegel[levels_spiegel$high]
# Kein Filme wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Braunschweig - Level 4 =======================
# Download der Daten:
terms_braunschweig = terms_spiegel[levels_spiegel$low]
gt_braunschweig = exportGT_anker(terms = terms_braunschweig,
                                 startdates = startdates_median[1],
                                 enddates = enddates[898],
                                 user = "sl.consulting.ws@gmail.com",
                                 pw = "consulting2016", anker = "Braunschweig")

# Hinzufügen von Variablennamen zum Datensatz:
films_braunschweig = films_spiegel[levels_spiegel$low]
colnames(gt_braunschweig) [4:ncol(gt_braunschweig)] =
  gsub(films_braunschweig, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_braunschweig = end_forecast_week_spiegel[levels_spiegel$low]
levels_braunschweig = detect_low_level_terms(terms = terms_braunschweig,
                                             trends = gt_braunschweig,
                                             enddate = end_forecast_week_braunschweig)
terms_braunschweig[levels_braunschweig$high]
# Kein Film werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")

# Ziehung von Daten für Ankerwort Siegen - Level 5 =============================
# Download der Daten:
terms_siegen = terms_braunschweig[levels_braunschweig$low]
gt_siegen = exportGT_anker(terms = terms_siegen, startdates = startdates_median[1],
                           enddates = enddates[898],
                           user = "sl.consulting.ws@gmail.com",
                           pw = "consulting2016", anker = "Siegen")

# Hinzufügen von Variablennamen zum Datensatz:
films_siegen = films_braunschweig[levels_braunschweig$low]
colnames(gt_siegen)[4:ncol(gt_siegen)] = 
  gsub(films_siegen, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_siegen = end_forecast_week_braunschweig[levels_braunschweig$low]
levels_siegen = detect_low_level_terms(terms = terms_siegen,
                                       trends = gt_siegen,
                                       enddate = end_forecast_week_siegen)
terms_siegen[levels_siegen$high]
# 1 Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Plauen - Level 6 =============================
# Download der Daten:
terms_plauen = terms_siegen[levels_siegen$low]
gt_plauen = exportGT_anker(terms = terms_plauen, startdates = startdates_median[1],
                           enddates = enddates[898],
                           user = "sl.consulting.ws@gmail.com",
                           pw = "consulting2016", anker = "Plauen")

# Hinzufügen von Variablennamen zum Datensatz:
films_plauen = films_siegen[levels_siegen$low]
colnames(gt_plauen)[4:ncol(gt_plauen)] = 
  gsub(films_plauen, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_plauen = end_forecast_week_siegen[levels_siegen$low]
levels_plauen = detect_low_level_terms(terms = terms_plauen,
                                       trends = gt_plauen,
                                       enddate = end_forecast_week_plauen)
terms_plauen[levels_plauen$high]
# 5 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("data/gt-data_anker/gt-data_anker_main_title.RData")


# Ziehung von Daten für Ankerwort Bad Hersfeld - Level 7 =======================
# Download der Daten:
terms_badhersfeld = terms_plauen[levels_plauen$low]
gt_badhersfeld = exportGT_anker(terms = terms_badhersfeld,
                                startdates = startdates[1],
                                enddates = enddates[898],
                                user = "sl.consulting.ws@gmail.com",
                                pw = "consulting2016", anker = "Bad Hersfeld")

# Hinzufügen von Variablennamen zum Datensatz:
films_badhersfeld = films_plauen[levels_plauen$low]
colnames(gt_badhersfeld)[4:ncol(gt_badhersfeld)] = 
  gsub(films_badhersfeld, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_badhersfeld = end_forecast_week_plauen[levels_plauen$low]
levels_badhersfeld = detect_low_level_terms(terms = terms_badhersfeld,
                                            trends = gt_badhersfeld,
                                            enddate = end_forecast_week_badhersfeld)
terms_badhersfeld[levels_badhersfeld$high]
# 17 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Reinbek - Level 8 =======================
# Download der Daten:
terms_reinbek = terms_badhersfeld[levels_badhersfeld$low]
gt_reinbek = exportGT_anker(terms = terms_reinbek, startdates = startdates[1],
                            enddates = enddates[898],
                            user = "sl.consulting.ws@gmail.com",
                            pw = "consulting2016", anker = "Reinbek")

# Hinzufügen von Variablennamen zum Datensatz:
films_reinbek = films_badhersfeld[levels_badhersfeld$low]
colnames(gt_reinbek)[4:ncol(gt_reinbek)] = 
  gsub(films_reinbek, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_reinbek = end_forecast_week_badhersfeld[levels_badhersfeld$low]
levels_reinbek = detect_low_level_terms(terms = terms_reinbek,
                                        trends = gt_reinbek,
                                        enddate = end_forecast_week_reinbek)
terms_reinbek[levels_reinbek$high]
# 46 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Nordkurier - Level 9 =======================
# Download der Daten:
terms_nordkurier = terms_reinbek[levels_reinbek$low]
gt_nordkurier = exportGT_anker(terms = terms_nordkurier[707:710],
                               startdates = startdates[1],
                               enddates = enddates[898],
                               user = "sl.consulting.ws@gmail.com",
                               pw = "consulting2016", anker = "Nordkurier")

# Hinzufügen von Variablennamen zum Datensatz:
films_nordkurier = films_reinbek[levels_reinbek$low]
colnames(gt_nordkurier)[4:ncol(gt_nordkurier)] = 
  gsub(films_nordkurier, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_nordkurier = end_forecast_week_reinbek[levels_reinbek$low]
levels_nordkurier = detect_low_level_terms(terms = terms_nordkurier,
                                           trends = gt_nordkurier,
                                           enddate = end_forecast_week_nordkurier)
terms_nordkurier[levels_nordkurier$high]
# 61 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Allgaeuer Zeitung - Level 10 ==================
# Download der Daten:
terms_allgaeuerzeitung = terms_nordkurier[levels_nordkurier$low]
gt_allgaeuerzeitung = exportGT_anker(terms = terms_allgaeuerzeitung[1:400],
                                     startdates = startdates[1],
                                     enddates = enddates[898],
                                     user = "sl.consulting.ws@gmail.com",
                                     pw = "consulting2016",
                                     anker = "Allgäuer Zeitung")

# Hinzufügen von Variablennamen zum Datensatz:
films_allgaeuerzeitung = films_nordkurier[levels_nordkurier$low]
colnames(gt_allgaeuerzeitung)[4:ncol(gt_allgaeuerzeitung)] = 
  gsub(films_allgaeuerzeitung, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_allgaeuerzeitung = end_forecast_week_nordkurier[levels_nordkurier$low]
levels_allgaeuerzeitung = detect_low_level_terms(terms = terms_allgaeuerzeitung,
                                                 trends = gt_allgaeuerzeitung,
                                                 enddate = end_forecast_week_allgaeuerzeitung)
terms_allgaeuerzeitung[levels_allgaeuerzeitung$high]
# 163 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Fraenkische Nachrichten - Level 11 ============
# Download der Daten:
terms_fraenkischenachrichten = terms_allgaeuerzeitung[levels_allgaeuerzeitung$low]
gt_fraenkischenachrichten = exportGT_anker(terms = terms_fraenkischenachrichten,
                                           startdates = startdates[1],
                                           enddates = enddates[898],
                                           user = "sl.consulting.ws@gmail.com",
                                           pw = "consulting2016",
                                           anker = "Fränkische Nachrichten")


# Hinzufügen von Variablennamen zum Datensatz:
films_fraenkischenachrichten = films_allgaeuerzeitung[levels_allgaeuerzeitung$low]
colnames(gt_fraenkischenachrichten)[4:ncol(gt_fraenkischenachrichten)] = 
  gsub(films_fraenkischenachrichten, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_fraenkischenachrichten = end_forecast_week_allgaeuerzeitung[levels_allgaeuerzeitung$low]
levels_fraenkischenachrichten = detect_low_level_terms(terms = terms_fraenkischenachrichten,
                                                       trends = gt_fraenkischenachrichten,
                                                       enddate = end_forecast_week_fraenkischenachrichten)
terms_fraenkischenachrichten[levels_fraenkischenachrichten$high]
# 90 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Ziehung von Daten für Ankerwort Schleswiger Nachrichten - Level 12 ===========
# Download der Daten:
terms_schleswigernachrichten = terms_fraenkischenachrichten[levels_fraenkischenachrichten$low]
gt_schleswigernachrichten = exportGT_anker(terms = terms_schleswigernachrichten,
                                           startdates = startdates[1],
                                           enddates = enddates[898],
                                           user = "sl.consulting.ws@gmail.com",
                                           pw = "consulting2016",
                                           anker = "Schleswiger Nachrichten")

# Hinzufügen von Variablennamen zum Datensatz:
films_schleswigernachrichten = films_fraenkischenachrichten[levels_fraenkischenachrichten$low]
colnames(gt_schleswigernachrichten)[4:ncol(gt_schleswigernachrichten)] = 
  gsub(films_schleswigernachrichten, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_schleswigernachrichten = end_forecast_week_fraenkischenachrichten[levels_fraenkischenachrichten$low]
levels_schleswigernachrichten = detect_low_level_terms(terms = terms_schleswigernachrichten,
                                                       trends = gt_schleswigernachrichten,
                                                       enddate = end_forecast_week_schleswigernachrichten)
terms_schleswigernachrichten[levels_schleswigernachrichten$high]
# 153 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")

# Herunterskalierung der Filme: ================================================

# Laden des Workspaces mit den Google Trends-Daten:
load("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")
load("01 Daten/gt-Daten_anker/gt-Daten_ankerworte.RData")

# Level "Hamburg Median" zu Hamburg:
gt_hamburg_scaled = scale_down(upper_gt = gt_hamburg,
                               lower_gt = gt_hamburg,
                               scale_gt = gt_hamburg_median)

# Level "Hamburg" zu Level "Frankfurt":
gt_frankfurt_scaled = scale_down(upper_gt = gt_hamburg,
                                 lower_gt = gt_frankfurt,
                                 scale_gt = gt_hamburg_frankfurt)

# Level "Frankfurt" zu Level "Spiegel":
gt_spiegel_scaled = scale_down(upper_gt = gt_frankfurt_scaled,
                               lower_gt = gt_spiegel,
                               scale_gt = gt_hamburg_spiegel)

# Level "Spiegel" zu Level "Braunschweig":
gt_braunschweig_scaled = scale_down(upper_gt = gt_spiegel_scaled,
                                    lower_gt = gt_braunschweig,
                                    scale_gt = gt_hamburg_braunschweig)

# Level "Braunschweig" zu Level "Siegen":
gt_siegen_scaled = scale_down(upper_gt = gt_braunschweig_scaled,
                              lower_gt = gt_siegen,
                              scale_gt = gt_hamburg_siegen)

# Level "Siegen" zu Level "Plauen":
gt_plauen_scaled = scale_down(upper_gt = gt_siegen_scaled,
                              lower_gt = gt_plauen,
                              scale_gt = gt_hamburg_plauen)

# Level "Plauen" zu Level "Bad Hersfeld":
gt_badhersfeld_scaled = scale_down(upper_gt = gt_plauen_scaled,
                                   lower_gt = gt_badhersfeld,
                                   scale_gt = gt_hamburg_badhersfeld)

# Level "Bad Hersfeld" zu Level "Reinbek":
gt_reinbek_scaled = scale_down(upper_gt = gt_badhersfeld_scaled,
                               lower_gt = gt_reinbek,
                               scale_gt = gt_hamburg_reinbek)

# Level "Reinbek" zu Level "Nordkurier":
gt_nordkurier_scaled = scale_down(upper_gt = gt_reinbek_scaled,
                                  lower_gt = gt_nordkurier,
                                  scale_gt = gt_hamburg_nordkurier)

# Level "Nordkurier" zu Level "Allgaeuer Zeitung":
gt_allgaeuerzeitung_scaled = scale_down(upper_gt = gt_nordkurier_scaled,
                                        lower_gt = gt_allgaeuerzeitung,
                                        scale_gt = gt_hamburg_allgaeuerzeitung)

# Level "Allgaeuer Zeitung" zu Level "Fraenkische Nachrichten":
gt_fraenkischenachrichten_scaled = scale_down(upper_gt = gt_allgaeuerzeitung_scaled,
                                              lower_gt = gt_fraenkischenachrichten,
                                              scale_gt = gt_hamburg_fraenkischenachrichten)

# Level "Fraenkische Nachrichten" zu Level "Schleswiger Nachrichten":
gt_schleswigernachrichten_scaled = scale_down(upper_gt = gt_fraenkischenachrichten_scaled,
                                              lower_gt = gt_schleswigernachrichten,
                                              scale_gt = gt_hamburg_schleswigernachrichten)

# Level "Schleswiger Nachrichten" zu Level "Selber Tagblatt":
gt_selbertagblatt_scaled = scale_down(upper_gt = gt_schleswigernachrichten_scaled,
                                      lower_gt = gt_selbertagblatt,
                                      scale_gt = gt_hamburg_selbertagblatt)

# Festlegung des finalen Google Trends-Datensatzes:
gt_data_main_title_film = gt_schleswigernachrichten_scaled
gt_data_main_title_film = as.data.frame(t(gt_data_main_title_film[,4:ncol(gt_data_main_title_film)]))
colnames(gt_data_main_title_film) = gt_hamburg[, 2]
rownames(gt_data_main_title_film) = 1:nrow(gt_data_main_title)
gt_data_main_title_film = cbind(movies$Filmtitel, gt_data_main_title_film)
head(gt_data_main_title_film)
rm(list = ls() [which(ls() != "gt_data_main_title_film")])

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")


# Complete Title: ==============================================================

# Herausfiltern aller Filme, für die sich "main_title" und "complete_title"
# unterscheiden:
films_complete_title = complete_title[which(main_title != complete_title)]
end_forecast_week = end_forecast_week[which(main_title != complete_title)]
# Der Haupttitel und der komplette Titel unterscheiden sich bei 195 Filmen.


# Ziehung von Daten für Ankerwort Hamburg - Level 1 ============================

# Download der Daten:
gt_hamburg = exportGT_anker(terms = films_complete_title,
                            startdates = startdates_median[1],
                            enddates = enddates[898],
                            user = "sl.consulting.ws@gmail.com",
                            pw = "consulting2016", anker = "Hamburg")

# Hinzufügen von Variablennamen zum Datensatz:
films_hamburg = films[main_title != complete_title]
colnames(gt_hamburg) [4:ncol(gt_hamburg)] = 
  gsub(films_hamburg, pattern = " ", replacement = "_")

# Auswahl der Filme, die beim naechsten Level nochmals gezogen werden müssen:
levels_hamburg = detect_low_level_terms(terms = films_complete_title,
                                        trends = gt_hamburg,
                                        enddate = end_forecast_week)
films_complete_title[levels_hamburg$high]
# Kein Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")

# Ziehung von Daten für Ankerwort Frankfurt - Level 2 ==========================
# Download der Daten:
terms_frankfurt = films_complete_title[levels_hamburg$low]
gt_frankfurt = exportGT_anker(terms = terms_frankfurt,
                              startdates = startdates_median[1],
                              enddates = enddates[898],
                              user = "sl.consulting.ws@gmail.com",
                              pw = "consulting2016", anker = "Frankfurt")

# Hinzufügen von Variablennamen zum Datensatz:
films_frankfurt = films_hamburg[levels_hamburg$low]
colnames(gt_frankfurt) [4:ncol(gt_frankfurt)] = 
  gsub(films_frankfurt, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_frankfurt = end_forecast_week[levels_hamburg$low]
levels_frankfurt = detect_low_level_terms(terms = terms_frankfurt,
                                          trends = gt_frankfurt,
                                          enddate = end_forecast_week_frankfurt)
terms_frankfurt[levels_frankfurt$high]
# Kein Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")

# Ziehung von Daten für Ankerwort Spiegel - Level 3 ============================
# Download der Daten:
terms_spiegel = terms_frankfurt[levels_frankfurt$low]
gt_spiegel = exportGT_anker(terms = terms_spiegel, 
                            startdates = startdates_median[1],
                            enddates = enddates[898],
                            user = "sl.consulting.ws@gmail.com",
                            pw = "consulting2016", anker = "Spiegel")

# Hinzufügen von Variablennamen zum Datensatz:
films_spiegel = films_frankfurt[levels_frankfurt$low]
colnames(gt_spiegel) [4:ncol(gt_spiegel)] = 
  gsub(films_spiegel, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_spiegel = end_forecast_week_frankfurt[levels_frankfurt$low]
levels_spiegel = detect_low_level_terms(terms = terms_spiegel,
                                        trends = gt_spiegel,
                                        enddate = end_forecast_week_spiegel)
terms_spiegel[levels_spiegel$high]
# Kein Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Braunschweig - Level 4 =======================
# Download der Daten:
terms_braunschweig = terms_spiegel[levels_spiegel$low]
gt_braunschweig = exportGT_anker(terms = terms_braunschweig,
                                 startdates = startdates_median[1],
                                 enddates = enddates[898],
                                 user = "sl.consulting.ws@gmail.com",
                                 pw = "consulting2016", anker = "Braunschweig")

# Hinzufügen von Variablennamen zum Datensatz:
films_braunschweig = films_spiegel[levels_spiegel$low]
colnames(gt_braunschweig) [4:ncol(gt_braunschweig)] =
  gsub(films_braunschweig, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_braunschweig = end_forecast_week_spiegel[levels_spiegel$low]
levels_braunschweig = detect_low_level_terms(terms = terms_braunschweig,
                                             trends = gt_braunschweig,
                                             enddate = end_forecast_week_braunschweig)
terms_braunschweig[levels_braunschweig$high]
# Kein Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")

# Ziehung von Daten für Ankerwort Siegen - Level 5 =============================
# Download der Daten:
terms_siegen = terms_braunschweig[levels_braunschweig$low]
gt_siegen = exportGT_anker(terms = terms_siegen, startdates = startdates_median[1],
                           enddates = enddates[898],
                           user = "sl.consulting.ws@gmail.com",
                           pw = "consulting2016", anker = "Siegen")

# Hinzufügen von Variablennamen zum Datensatz:
films_siegen = films_braunschweig[levels_braunschweig$low]
colnames(gt_siegen)[4:ncol(gt_siegen)] = 
  gsub(films_siegen, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_siegen = end_forecast_week_braunschweig[levels_braunschweig$low]
levels_siegen = detect_low_level_terms(terms = terms_siegen,
                                       trends = gt_siegen,
                                       enddate = end_forecast_week_siegen)
terms_siegen[levels_siegen$high]
# Kein Film wird aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Plauen - Level 6 =============================
# Download der Daten:
terms_plauen = terms_siegen[levels_siegen$low]
gt_plauen = exportGT_anker(terms = terms_plauen, startdates = startdates_median[1],
                           enddates = enddates[898],
                           user = "sl.consulting.ws@gmail.com",
                           pw = "consulting2016", anker = "Plauen")

# Hinzufügen von Variablennamen zum Datensatz:
films_plauen = films_siegen[levels_siegen$low]
colnames(gt_plauen)[4:ncol(gt_plauen)] = 
  gsub(films_plauen, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_plauen = end_forecast_week_siegen[levels_siegen$low]
levels_plauen = detect_low_level_terms(terms = terms_plauen,
                                       trends = gt_plauen,
                                       enddate = end_forecast_week_plauen)
terms_plauen[levels_plauen$high]
# 6 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Bad Hersfeld - Level 7 =======================
# Download der Daten:
terms_badhersfeld = terms_plauen[levels_plauen$low]
gt_badhersfeld = exportGT_anker(terms = terms_badhersfeld,
                                startdates = startdates_median[1],
                                enddates = enddates[898],
                                user = "sl.consulting.ws@gmail.com",
                                pw = "consulting2016", anker = "Bad Hersfeld")

# Hinzufügen von Variablennamen zum Datensatz:
films_badhersfeld = films_plauen[levels_plauen$low]
colnames(gt_badhersfeld)[4:ncol(gt_badhersfeld)] = 
  gsub(films_badhersfeld, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_badhersfeld = end_forecast_week_plauen[levels_plauen$low]
levels_badhersfeld = detect_low_level_terms(terms = terms_badhersfeld,
                                            trends = gt_badhersfeld,
                                            enddate = end_forecast_week_badhersfeld)
terms_badhersfeld[levels_badhersfeld$high]
# 5 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Reinbek - Level 8 =======================
# Download der Daten:
terms_reinbek = terms_badhersfeld[levels_badhersfeld$low]
gt_reinbek = exportGT_anker(terms = terms_reinbek, 
                            startdates = startdates_median[1],
                            enddates = enddates[898],
                            user = "sl.consulting.ws@gmail.com",
                            pw = "consulting2016", anker = "Reinbek")

# Hinzufügen von Variablennamen zum Datensatz:
films_reinbek = films_badhersfeld[levels_badhersfeld$low]
colnames(gt_reinbek)[4:ncol(gt_reinbek)] = 
  gsub(films_reinbek, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_reinbek = end_forecast_week_badhersfeld[levels_badhersfeld$low]
levels_reinbek = detect_low_level_terms(terms = terms_reinbek,
                                        trends = gt_reinbek,
                                        enddate = end_forecast_week_reinbek)
terms_reinbek[levels_reinbek$high]
# 22 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Nordkurier - Level 9 =======================
# Download der Daten:
terms_nordkurier = terms_reinbek[levels_reinbek$low]
gt_nordkurier = exportGT_anker(terms = terms_nordkurier,
                               startdates = startdates_median[1],
                               enddates = enddates[898],
                               user = "sl.consulting.ws@gmail.com",
                               pw = "consulting2016", anker = "Nordkurier")

# Hinzufügen von Variablennamen zum Datensatz:
films_nordkurier = films_reinbek[levels_reinbek$low]
colnames(gt_nordkurier)[4:ncol(gt_nordkurier)] = 
  gsub(films_nordkurier, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_nordkurier = end_forecast_week_reinbek[levels_reinbek$low]
levels_nordkurier = detect_low_level_terms(terms = terms_nordkurier,
                                           trends = gt_nordkurier,
                                           enddate = end_forecast_week_nordkurier)
terms_nordkurier[levels_nordkurier$high]
# 11 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Allgaeuer Zeitung - Level 10 ==================
# Download der Daten:
terms_allgaeuerzeitung = terms_nordkurier[levels_nordkurier$low]
gt_allgaeuerzeitung = exportGT_anker(terms = terms_allgaeuerzeitung,
                                     startdates = startdates_median[1],
                                     enddates = enddates[898],
                                     user = "sl.consulting.ws@gmail.com",
                                     pw = "consulting2016",
                                     anker = "Allgäuer Zeitung")

# Hinzufügen von Variablennamen zum Datensatz:
films_allgaeuerzeitung = films_nordkurier[levels_nordkurier$low]
colnames(gt_allgaeuerzeitung)[4:ncol(gt_allgaeuerzeitung)] = 
  gsub(films_allgaeuerzeitung, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_allgaeuerzeitung = end_forecast_week_nordkurier[levels_nordkurier$low]
levels_allgaeuerzeitung = detect_low_level_terms(terms = terms_allgaeuerzeitung,
                                                 trends = gt_allgaeuerzeitung,
                                                 enddate = end_forecast_week_allgaeuerzeitung)
terms_allgaeuerzeitung[levels_allgaeuerzeitung$high]
# 21 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Fraenkische Nachrichten - Level 11 ============
# Download der Daten:
terms_fraenkischenachrichten = terms_allgaeuerzeitung[levels_allgaeuerzeitung$low]
gt_fraenkischenachrichten = exportGT_anker(terms = terms_fraenkischenachrichten,
                                           startdates = startdates_median[1],
                                           enddates = enddates[898],
                                           user = "sl.consulting.ws@gmail.com",
                                           pw = "consulting2016",
                                           anker = "Fränkische Nachrichten")

# Hinzufügen von Variablennamen zum Datensatz:
films_fraenkischenachrichten = films_allgaeuerzeitung[levels_allgaeuerzeitung$low]
colnames(gt_fraenkischenachrichten)[4:ncol(gt_fraenkischenachrichten)] = 
  gsub(films_fraenkischenachrichten, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_fraenkischenachrichten = end_forecast_week_allgaeuerzeitung[levels_allgaeuerzeitung$low]
levels_fraenkischenachrichten = detect_low_level_terms(terms = terms_fraenkischenachrichten,
                                                       trends = gt_fraenkischenachrichten,
                                                       enddate = end_forecast_week_fraenkischenachrichten)
terms_fraenkischenachrichten[levels_fraenkischenachrichten$high]
# 56 Filme werden aussortiert.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Ziehung von Daten für Ankerwort Schleswiger Nachrichten - Level 12 ===========
# Download der Daten:
terms_schleswigernachrichten = terms_fraenkischenachrichten[levels_fraenkischenachrichten$low]
gt_schleswigernachrichten = exportGT_anker(terms = terms_schleswigernachrichten,
                                           startdates = startdates_median[1],
                                           enddates = enddates[898],
                                           user = "sl.consulting.ws@gmail.com",
                                           pw = "consulting2016",
                                           anker = "Schleswiger Nachrichten")

# Hinzufügen von Variablennamen zum Datensatz:
films_schleswigernachrichten = films_fraenkischenachrichten[levels_fraenkischenachrichten$low]
colnames(gt_schleswigernachrichten)[4:ncol(gt_schleswigernachrichten)] = 
  gsub(films_schleswigernachrichten, pattern = " ", replacement = "_")

# Auswahl von Filmen, die beim naechsten Level nochmals gezogen werden müssen:
end_forecast_week_schleswigernachrichten = end_forecast_week_fraenkischenachrichten[levels_fraenkischenachrichten$low]
levels_schleswigernachrichten = detect_low_level_terms(terms = terms_schleswigernachrichten,
                                                       trends = gt_schleswigernachrichten,
                                                       enddate = end_forecast_week_schleswigernachrichten)
terms_schleswigernachrichten[levels_schleswigernachrichten$high]
# 32 Filme werden aussortiert.
# Für 42 Filme ist keine genaue Bestimmung der Google-Suchanfragen möglich.

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")

# Herunterskalierung der Filme: ================================================

# Laden des Workspaces mit den Google Trends-Daten:
load("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")
load("01 Daten/gt-Daten_anker/gt-Daten_ankerworte.RData")

# Level "Hamburg Median" zu Hamburg:
gt_hamburg_scaled = scale_down(upper_gt = gt_hamburg,
                               lower_gt = gt_hamburg,
                               scale_gt = gt_hamburg_median)

# Level "Hamburg" zu Level "Frankfurt":
gt_frankfurt_scaled = scale_down(upper_gt = gt_hamburg,
                                 lower_gt = gt_frankfurt,
                                 scale_gt = gt_hamburg_frankfurt)

# Level "Frankfurt" zu Level "Spiegel":
gt_spiegel_scaled = scale_down(upper_gt = gt_frankfurt_scaled,
                               lower_gt = gt_spiegel,
                               scale_gt = gt_hamburg_spiegel)

# Level "Spiegel" zu Level "Braunschweig":
gt_braunschweig_scaled = scale_down(upper_gt = gt_spiegel_scaled,
                                    lower_gt = gt_braunschweig,
                                    scale_gt = gt_hamburg_braunschweig)

# Level "Braunschweig" zu Level "Siegen":
gt_siegen_scaled = scale_down(upper_gt = gt_braunschweig_scaled,
                              lower_gt = gt_siegen,
                              scale_gt = gt_hamburg_siegen)

# Level "Siegen" zu Level "Plauen":
gt_plauen_scaled = scale_down(upper_gt = gt_siegen_scaled,
                              lower_gt = gt_plauen,
                              scale_gt = gt_hamburg_plauen)

# Level "Plauen" zu Level "Bad Hersfeld":
gt_badhersfeld_scaled = scale_down(upper_gt = gt_plauen_scaled,
                                   lower_gt = gt_badhersfeld,
                                   scale_gt = gt_hamburg_badhersfeld)

# Level "Bad Hersfeld" zu Level "Reinbek":
gt_reinbek_scaled = scale_down(upper_gt = gt_badhersfeld_scaled,
                               lower_gt = gt_reinbek,
                               scale_gt = gt_hamburg_reinbek)

# Level "Reinbek" zu Level "Nordkurier":
gt_nordkurier_scaled = scale_down(upper_gt = gt_reinbek_scaled,
                                  lower_gt = gt_nordkurier,
                                  scale_gt = gt_hamburg_nordkurier)

# Level "Nordkurier" zu Level "Allgaeuer Zeitung":
gt_allgaeuerzeitung_scaled = scale_down(upper_gt = gt_nordkurier_scaled,
                                        lower_gt = gt_allgaeuerzeitung,
                                        scale_gt = gt_hamburg_allgaeuerzeitung)

# Level "Allgaeuer Zeitung" zu Level "Fraenkische Nachrichten":
gt_fraenkischenachrichten_scaled = scale_down(upper_gt = gt_allgaeuerzeitung_scaled,
                                              lower_gt = gt_fraenkischenachrichten,
                                              scale_gt = gt_hamburg_fraenkischenachrichten)

# Level "Fraenkische Nachrichten" zu Level "Schleswiger Nachrichten":
gt_schleswigernachrichten_scaled = scale_down(upper_gt = gt_fraenkischenachrichten_scaled,
                                              lower_gt = gt_schleswigernachrichten,
                                              scale_gt = gt_hamburg_schleswigernachrichten)

# Level "Schleswiger Nachrichten" zu Level "Selber Tagblatt":
gt_selbertagblatt_scaled = scale_down(upper_gt = gt_schleswigernachrichten_scaled,
                                      lower_gt = gt_selbertagblatt,
                                      scale_gt = gt_hamburg_selbertagblatt)

# Festlegung des finalen Google Trends-Datensatzes:
gt_Daten_complete_title = gt_schleswigernachrichten_scaled
gt_Daten_complete_title = as.data.frame(t(gt_Daten_complete_title[,4:ncol(gt_Daten_complete_title)]))
rownames(gt_Daten_complete_title) = 1:nrow(gt_Daten_complete_title)
gt_Daten_complete_title = cbind(movies$Filmtitel[which(complete_title != main_title)],
                                gt_Daten_complete_title)
rownames(gt_Daten_complete_title) = 1:nrow(gt_Daten_complete_title)
colnames(gt_Daten_complete_title) = colnames(gt_data_main_title)
gt_Daten_complete_title = rbind(gt_data_main_title[which(complete_title == main_title), ],
                                gt_Daten_complete_title)
gt_Daten_complete_title = gt_Daten_complete_title[order(match(gt_Daten_complete_title[, 1],
                                                              movies$Filmtitel)), ]
rownames(gt_Daten_complete_title) = 1:nrow(gt_Daten_complete_title)
rm(list = ls() [which(ls() != "gt_Daten_complete_title")])

# Abspeichern des Workspaces:
save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")


# Verbinden der Workspaces und der Kino-Daten ==================================
# Abspeichern des finalen Workspaces:
load("data/gt-data_anker/gt-data_anker_main_title.RData")
load("01 Daten/gt-Daten_anker/gt-Daten_anker_main_title_film.RData")
load("01 Daten/gt-Daten_anker/gt-Daten_anker_complete_title.RData")

gconnect("sl.consulting.ws@gmail.com", "consulting2016")
gt_kino = gtrends("Kino", geo = "DE", 
                  start_date = as.character(startdates[1]),
                  end_date = as.character(enddates[898]))$trend

save.image("01 Daten/gt-Daten_anker/gt-Daten_anker_final.RData")

# Die ersten 235 Filme mussten aufgrund der nachtraeglich durchgefuehrten
# Medianbereinigung noch einmal mit frueheren Startdaten gezogen werden. Die
# Datenziehung erfolgte analog zu der in diesem Skript durchgeführten
# Datengenerierung. Somit ergaben sich drei weitere Datensaetze, die unter
# gt-Daten_anker_final_median gespeichert sind.
