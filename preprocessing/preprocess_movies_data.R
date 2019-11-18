source("utils/preprocessing_utils.R")

movies = read.csv("data/movies.csv", sep = ";", dec = ",", comment.char = "", encoding = "UTF-8", na.strings = "")
movies$premiere_date = as.POSIXct(movies$premiere_date, format = "%d.%m.%y", tz = "GMT")
movies$title = as.character(movies$title)
movies$genre = as.character(movies$genre)
movies$rating = as.factor(movies$rating)
movies$kids_movie = as.factor(ifelse(test = movies$kids_movie == "yes", yes = 1, no = 0))

# Add studio category
major = c("20th Century Fox", "Paramount Pictures", "Sony Pictures Releasing", "Universal Pictures", "Walt Disney",
          "Warner Bros.")
major_independent = c("Concorde", "Constantin Film", "DCM", "Majestic (Fox)", "NFP (Warner)", "Senator", "Studiocanal",
                      "Tobis", "Universum Film", "Wild Bunch", "X Verleih")
independent = c("Alamode Film", "Alpenrepublik", "Arsenal (Central)", "Arts Alliance", "Ascot Elite", "Barnsteiner",
                "Camino","Capelight", "Central Film", "Delphi Filmverleih", "Falcom", "Farbfilm", "Koch Media", 
                "KSM GmbH", "Maerchenfilm", "MFA+ Filmdistribution", "Movienet", "Neue Visionen", "NFP", "Pandastorm",
                "Pandora", "polyband", "Port-au-Prince", "Pro-Fun", "Prokino", "Splendid", "SpotOn Distribution",
                "Studio Hamburg", "Summiteer Films","Tiberius Film", "Weltkino", "Zorro")
movies$studio = as.factor(ifelse(movies$studio %in% major, "major",
                                 ifelse(movies$studio %in% major_independent, "major_independent", "independent")))

movies$visitors_premiere_weekend_log = log(movies$visitors_premiere_weekend)
movies$visitors_overall_log = log(movies$visitors_overall)

# Add forecast horizon dates
movies$forecast_start = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 7)) + 
  as.difftime(tim = 1, units = "days")
movies$forecast_date6 = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 6))
movies$forecast_date5 = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 5))
movies$forecast_date4 = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 4))
movies$forecast_date3 = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 3))
movies$forecast_date2 = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 2))
movies$forecast_date1 = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 1))

# Replace "&" with "und" for german movies and "and" for english ones
movies$title[c(67, 95, 151, 236, 559, 608, 621, 826, 836, 846)] = 
  gsub(movies$title[c(67, 95, 151, 236, 559, 608, 621, 826, 836, 846)], pattern = "&", replacement = "and")
movies$title[c(84, 97, 280, 286, 293, 486, 506, 727, 771, 894)] = 
  gsub(movies$title[c(84, 97, 280, 286, 293, 486, 506, 727, 771, 894)], pattern = "&", replacement = "und")

# Correct some movvie titles:
movies$title[788] = "Tschiller: Off Duty"
movies$title[572] = "Dessau Dancers"

# Remove "Eine Taube sitzt auf einem Zweig und denkt über das Leben nach" und "#Zeitgeist" aus dem Datensatz:
movies = movies[-491, ]
movies = movies[-479,]

# Get end of forecast horizon
last_forecast_date = do.call(what = c, args = lapply(X = movies$premiere_date, FUN = forecast_date, forecast = 1))
last_forecast_date = trunc(x = last_forecast_date, units = "days")

# Define searchterms for Google Trends
main_title = define_searchterms(movies$title)[[1]]
main_title_film = define_searchterms(movies$title)[[2]]
complete_title = define_searchterms(movies$title)[[3]]
movies$main_title = main_title
movies$secondary_title = as.factor(ifelse(test = main_title == complete_title, yes = 0, no = 1))

movies$words = sapply(X = strsplit(x = movies$main_title, split = " "), FUN = length)
movies$month = factor(strftime(movies$premiere_date, format = "%b"))
movies$month = factor(movies$month, levels = unique(movies$month))

movies$season = as.factor(ifelse(test = movies$month %in% c("Okt", "Nov", "Dez", "Jan", "Feb", "Mär"), 
                                 yes = "winter", no = "summer"))
movies$year_even = as.numeric(strftime(x = movies$premiere_date, format = "%y"))
movies$odd_year = as.factor(ifelse(test = movies$year_even %% 2 == 0, yes = 1, no = 0))

# Concatenate genres "Fantasy" and "Teenager-Film":
movies$genre[movies$genre == "Fantasy"] = "TeenagerMovie_Fantasy"
movies$genre[movies$genre == "TeenagerMovie"] = "TeenagerMovie_Fantasy"
movies$genre = as.factor(movies$genre)

remove(major, major_independent, independent, forecast_date, calculate_box_cox,
       calculate_google_value, calculate_median, cross_validate_inner, define_searchterms, estimate_box_cox,
       optim_weights, select_gt_data)
save.image("data/movies_preprocessed.RData")
