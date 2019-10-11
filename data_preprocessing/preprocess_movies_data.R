# Setzen des Workspaces
setwd("..")

# Zugriff auf benötigte Programme:
source("02 Programme/Funktionen/forecast_date.R")
source("02 Programme/Funktionen/define_searchterms.R")

# Einlesen des Datensatzes:
movies = read.csv("01 Daten/Movies.csv", sep = ";", dec = ",",
                   comment.char = "", encoding = "UTF-8", na.strings = "")

# Korrekte Benennung der Variablen::
names(movies) = c("Kinostart", "Filmtitel", "Verleiher", "Kinderfilm", "FSK",
                   "Genre_Prognose", "Genre1", "Genre2", "Kopien",
                   "Besucher_wochenende1", "Kopienschnitt",
                   "Besucher_gesamt", "Tipp_min6", "Tipp_max6", "Tipp_min5",
                   "Tipp_max5", "Tipp_min4", "Tipp_max4", "Tipp_min3",
                   "Tipp_max3", "Tipp_min2", "Tipp_max2", "Tipp_min1",
                   "Tipp_max1", "Tippspiel", "Awareness_unaided6",
                   "Awareness_unaided5", "Awareness_unaided4",
                   "Awareness_unaided3", "Awareness_unaided2",
                   "Awareness_unaided1", "Awareness_aided6",
                   "Awareness_aided5", "Awareness_aided4", "Awareness_aided3",
                   "Awareness_aided2", "Awareness_aided1", "First_choice6",
                   "First_choice5", "First_choice4", "First_choice3",
                   "First_choice2", "First_choice1")

# Umkodieren einzelner Variablen:
movies$Filmtitel = as.character(movies$Filmtitel)
movies$FSK = as.factor(movies$FSK)
movies$Kinostart = as.POSIXct(movies$Kinostart, format = "%d.%m.%y",
                               tz = "GMT")
movies$Kinderfilm = as.factor(ifelse(test = movies$Kinderfilm == "ja",
                                      yes = 1, no = 0))

# Hinzufügen der Variable "Verleiherkategorie":
major = c("20th Century Fox", "Paramount Pictures", "Sony Pictures Releasing",
           "Universal Pictures", "Walt Disney", "Warner Bros.")
major_independent = c("Concorde", "Constantin Film", "DCM", "Majestic (Fox)",
                       "NFP (Warner)", "Senator", "Studiocanal", "Tobis",
                       "Universum Film", "Wild Bunch", "X Verleih")
independent = c("Alamode Film", "Alpenrepublik", "Arsenal (Central)",
                 "Arts Alliance", "Ascot Elite", "Barnsteiner", "Camino",
                 "Capelight", "Central Film", "Delphi Filmverleih",
                 "Falcom", "Farbfilm", "Koch Media", "KSM GmbH",
                 "Maerchenfilm", "MFA+ Filmdistribution", "Movienet",
                 "Neue Visionen", "NFP", "Pandastorm", "Pandora", "polyband",
                 "Port-au-Prince", "Pro-Fun", "Prokino", "Splendid",
                 "SpotOn Distribution", "Studio Hamburg", "Summiteer Films",
                 "Tiberius Film", "Weltkino", "Zorro")

movies$Verleiherkategorie = as.factor(ifelse(movies$Verleiher %in% major, "major", 
                                   ifelse(movies$Verleiher %in% major_independent, 
                                          "major_independent", "independent")))

# Hinzufügen von logarithmierten Zielvariablen:
movies$Besucher_wochenende1_log = log(movies$Besucher_wochenende1)
movies$Besucher_gesamt_log = log(movies$Besucher_gesamt)

# Hinzufügen von Datumsvariablen, die die Prognosezeitpunkte angeben:
movies$Prognose_start = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 7)) +
  as.difftime(tim = 1, units = "days")
movies$Prognosedatum6 = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 6))
movies$Prognosedatum5 = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 5))
movies$Prognosedatum4 = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 4))
movies$Prognosedatum3 = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 3))
movies$Prognosedatum2 = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 2))
movies$Prognosedatum1 = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                         FUN = forecast_date,
                                                         forecast = 1))

# Ersetzen des kaufmännischen & in Filmtiteln durch "and" oder "und":
movies$Filmtitel[c(67, 95, 151, 236, 559, 608, 621, 826, 836, 846)] = 
  gsub(movies$Filmtitel[c(67, 95, 151, 236, 559, 608, 621, 826, 836, 846)],
       pattern = "&", replacement = "and")
movies$Filmtitel[c(84, 97, 280, 286, 293, 486, 506, 727, 771, 894)] = 
  gsub(movies$Filmtitel[c(84, 97, 280, 286, 293, 486, 506, 727, 771, 894)],
pattern = "&", replacement = "und")

# Korrektur der Filmtitel "Tschiller: Off Duty" und "Dessau Dancers":
movies$Filmtitel[788] = "Tschiller: Off Duty"
movies$Filmtitel[572] = "Dessau Dancers"

# Entfernen der Filme "Eine Taube sitzt auf einem Zweig und denkt über das
# Leben nach" und "#Zeitgeist" aus dem Datensatz:
movies = movies[-491, ]
movies = movies[-479,]

# Variable mit Enden der Prognosewochen (Datenziehung mit Ankerworten):
end_forecast_week = do.call(what = c, args = lapply(X = movies$Kinostart,
                                                     FUN = forecast_date,
                                                     forecast = 1))
end_forecast_week = trunc(x = end_forecast_week, units = "days")

# Definition der Suchbegriffe für Google-Trends:
main_title = define_searchterms(movies$Filmtitel)[[1]]
main_title_film = define_searchterms(movies$Filmtitel)[[2]]
complete_title = define_searchterms(movies$Filmtitel)[[3]]

# Hinzufügen des Haupttitels eines Films zum Datensatz:
movies$Haupttitel = main_title

# Hinzufügen einer Variable zum Datensatz, die angibt, ob Haupttitel und
# kompletter Filmtitel identisch sind:
movies$Untertitel = as.factor(ifelse(test = main_title == complete_title,
                                      yes = 0, no = 1))

# Hinzufügen zusätzlicher potentieller Einflussvariablen zum Datensatz:
movies$Woerter = sapply(X = strsplit(x = movies$Haupttitel, split = " "),
                         FUN = length)
movies$Monat = factor(strftime(movies$Kinostart, format = "%b"))
movies$Monat = factor(movies$Monat, levels = unique(movies$Monat))

movies$Saison = as.factor(ifelse(test = movies$Monat %in% c("Okt", "Nov",
                                                             "Dez", "Jan",
                                                             "Feb", "Mär"),
                                  yes = "Winter", no = "Sommer"))
movies$Jahr_gerade = as.numeric(strftime(x = movies$Kinostart, format = "%y"))
movies$Jahr_gerade = as.factor(ifelse(test = movies$Jahr_gerade %% 2 == 0,
                                      yes = 1, no = 0))

# Zusammenfügen der Prognosegenres "Fantasy" und "Teenager-Film":
levels(movies$Genre_Prognose) = c(levels(movies$Genre_Prognose), "Teenager-Film/Fantasy")
movies$Genre_Prognose[movies$Genre_Prognose == "Fantasy"] = "Teenager-Film/Fantasy"
movies$Genre_Prognose[movies$Genre_Prognose == "Teenager-Film"] = "Teenager-Film/Fantasy"
movies$Genre_Prognose = droplevels(movies$Genre_Prognose)

# Löschen von nicht mehr benötigten Objekten aus dem Workspace:
remove(major, major_independent, independent, forecast_date, define_searchterms)
save.image("01 Daten/movies_aufbereitet.RData")



