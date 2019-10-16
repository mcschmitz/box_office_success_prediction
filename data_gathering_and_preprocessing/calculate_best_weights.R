# Setzen des Workspaces
setwd("..")

# Laden von benötigten Paketen:
require(parallel)
#source("02 Programme/Funktionen/transform_box_cox.R")
source("02 Programme/Funktionen/estimate_weights.R")
source("02 Programme/Funktionen/estimate_box_cox.R")

# Laden des Workspaces mit abgespeicherten Daten aus Google Trends sowie des
# Workspaces mit den Kinodaten:
load("data/movies_preprocessed.RData")

# Nested Resampling wochenweise ausführen
## für einzel Daten
for (horizon in 1:6){
  set.seed(horizon)
  data_sub = data_final[, c(46, 65 - horizon, 89 - horizon, 77 - horizon, 55)]
  cat("Starte Berechnung für Woche", horizon, "mit folgenden Variablen:",
      paste0(colnames(data_sub), collapse = ","), "\n")
  values = optim_weights(data = data_sub, n_outer = 3, n_inner = 10,
                         reps_inner = 5)
  assign(paste0("weights", horizon), values)
  gc()
  save.image("best_weights.Rdata")
}
## für aggregierte Daten
for (horizon in 1:6){
  set.seed(horizon)
  data_sub = data_final[, c(46, 74 - horizon, 98 - horizon, 86 - horizon, 56)]
  cat("Starte Berechnung für Woche", horizon, "mit folgenden Variablen:",
      paste0(colnames(data_sub), collapse = ","), "\n")
  values = optim_weights(data = data_sub, n_outer = 3, n_inner = 10,
                         reps_inner = 5)
  assign(paste0("weights_agg",horizon), values)
  gc()
  save.image("best_weights.Rdata")
}

# Weights wochenweise mitteln, um die finalen Gewichtungs-Parameter fuer die 
# Linnearkombination zu ersthalten
weights = matrix(ncol = 12, nrow = 2)
for(i in 1:6){
  current_weights = unlist(get(paste0("weights", i))[[1]])
  weights[1, 7 - i] = mean(current_weights[c(1, 3, 5)])
  weights[2, 7 - i] = mean(current_weights[c(2, 4, 6)])
  
  current_weights = unlist(get(paste0("weights_agg", i))[[1]])
  weights[1, 13 - i] = mean(current_weights[c(1, 3, 5)])
  weights[2, 13 - i] = mean(current_weights[c(2, 4, 6)])
}
rownames(weights) <- c("Gewichtung Haupttitel Film",
                       "Gewichtung Haupt- und Untertitel")
colnames(weights) = c(paste0("Woche", 6:1), paste0("Aggregation", 6:1))
(weights = weights/length(w_m))

remove(weights1, weights2, weights3, weights4, weights5, weights6,
       weights_agg1, weights_agg2, weights_agg3, weights_agg4, weights_agg5,
       weights_agg6, i, calculate_box_cox, cross_validate_inner, 
       estimate_box_cox, optim_weights, data_final, data_sub, lambda,
       current_weights, horizon, values)

save.image("03 Ergebnisse/Parameter/weights.Rdata")