require(MASS)
require(goftest)

load("data/data_final/data_final.RData")

# Target
truehist(data = data_final$Besucher_wochenende1, col = "grey", ylim = c(0, 0.00001), 
         xlab = "Visitors on the first weekend", ylab = "Density")
lines(x = density(data_final$Besucher_wochenende1), col = "black", lwd = 2)
grid = seq(from = 0, to = max(data_final$Besucher_wochenende1), by = 10)
lines(x = grid, y = dnorm(x = grid, mean = mean(data_final$Besucher_wochenende1), 
                          sd = sd(data_final$Besucher_wochenende1)), col = "red")

truehist(data = data_final$Besucher_wochenende1_log, col = "grey95", ylim = c(0.00, 0.25), ylab = "Density",
         xlab = "log(Visitors on the first weekend)")
lines(x = density(data_final$Besucher_wochenende1_log), col = "black", lwd = 2)
grid = seq(from = 0, to = max(data_final$Besucher_wochenende1_log) + 1, by = 0.1)
lines(x = grid, y = dnorm(x = grid, mean = mean(data_final$Besucher_wochenende1_log), 
                          sd = sd(data_final$Besucher_wochenende1_log)), col = "red3", lwd = 2)
legend("topright", legend = c("KDE", "Normaldistribution"), col = (c("black", "red3")), lty = 1)

shape = mean(data_final$Besucher_wochenende1) ^ 2 / var(data_final$Besucher_wochenende1)
scale = var(data_final$Besucher_wochenende1) / mean(data_final$Besucher_wochenende1)
truehist(data = data_final$Besucher_wochenende1, col = "grey95", ylim = c(0, 0.00001), ylab = "Density",
         xlab = "Visitors on the first weekend")
lines(x = density(data_final$Besucher_wochenende1), col = "black", lwd = 2)
grid = seq(from = 0, to = max(data_final$Besucher_wochenende1) + 1, by = 0.1)
y = dgamma(x = grid, shape = shape, scale = scale)
lines(x = grid, y = y, col = "red3", lwd = 2)
legend("topright", legend = c("KDE", "Gamma Distribution"), col = (c("black", "red3")), lty = 1)

# Check for normal distribution
fit_normal = fitdistr(x = data_final$Besucher_wochenende1, densfun = "normal")
# Kolmogorov-Smirnov-Test:
ks.test(x = data_final$Besucher_wochenende1, y = "pnorm", mean = fit_normal$estimate[1], sd = fit_normal$estimate[2],
        exact = TRUE)
# Anderson-Darling-Test:
ad.test(x = data_final$Besucher_wochenende1, null = "pnorm", mean = fit_normal$estimate[1], sd = fit_normal$estimate[2])
# Cramer-von Mises-Test:
cvm.test(x = data_final$Besucher_wochenende1, null = "pnorm", 
         mean = fit_normal$estimate[1], sd = fit_normal$estimate[2])
# => Decline Nul Hypothesis (normal distribution) for all tests sind p-value is below 0.05 for all tests


# Check for logarithmic normal distribution
fit_lognormal = fitdistr(x = data_final$Besucher_wochenende1, densfun = "lognormal")
# Kolmogorov-Smirnov-Test:
ks.test(x = data_final$Besucher_wochenende1_log, y = "pnorm", 
        mean = fit_lognormal$estimate[1], sd = fit_lognormal$estimate[2], exact = TRUE)
# Anderson-Darling-Test:
ad.test(x = data_final$Besucher_wochenende1_log, null = "pnorm", 
        mean = fit_lognormal$estimate[1], sd = fit_lognormal$estimate[2])
# Cramer-von Mises-Test:
cvm.test(x = data_final$Besucher_wochenende1_log, null = "pnorm", 
         mean = fit_lognormal$estimate[1], sd = fit_lognormal$estimate[2])
# => Accept Null Hypothesis (logarithmic normal distribution) for all tests sind p-value is above 0.05 for all tests

# Check for Gamma distribution
shape = mean(data_final$Besucher_wochenende1) ^ 2 / var(data_final$Besucher_wochenende1)
scale = var(data_final$Besucher_wochenende1) / mean(data_final$Besucher_wochenende1)
fit_gamma = fitdistr(x = data_final$Besucher_wochenende1, densfun = "gamma", start = list(shape = shape, scale = scale))
# Kolmogorov-Smirnov-Test:
ks.test(x = data_final$Besucher_wochenende1, y = "pgamma", shape = fit_gamma$estimate[1], scale = fit_gamma$estimate[2],
        exact = TRUE)
# Anderson-Darling-Test:
ad.test(x = data_final$Besucher_wochenende1, null = "pgamma", 
        shape = fit_gamma$estimate[1], scale = fit_gamma$estimate[2])
# Cramer-von Mises-Test:
cvm.test(x = data_final$Besucher_wochenende1, null = "pgamma", 
         scale = fit_gamma$estimate[1], shape = fit_gamma$estimate[2])
# => Decline Nul Hypothesis (gamma distribution) for all tests sind p-value is below 0.05 for all tests

# Boxlpots
par(mfrow = c(1, 2))
boxplot(x = data_final$Besucher_wochenende1, ylab = "Visitors on the first weekend")
boxplot(x = data_final$Besucher_wochenende1_log, ylab = "log(Visitors on the first weekend)")
par(mfrow = c(1, 1))

# Plot logarithmic over time
t_mon = as.POSIXlt(data_final$Kinostart)$mon + 1
t_y = as.POSIXlt(data_final$Kinostart)$year + 1900
t = as.Date(paste0(t_mon, "-", t_y, "01"), format = "%m-%Y%d")
tmp = aggregate(data_final$Besucher_wochenende1_log, by = list(t), mean)
colnames(tmp) = c("Date", "Visitors_log")
plot(tmp$Visitors_log ~ tmp$Date, type = "l", xaxt = "n", ylab =  "log(Visitors on the first weekend)", xlab = "Date")
axis.Date(side = 1, at = tmp$Date, format = "%m-%Y")

# Relation between number of copies and visistors
plot(x = data_final$Kopien, y = data_final$Besucher_wochenende1_log,
     main = "Relation between number off copies and visistors on the first weekend", xlab = "#Copies", 
     ylab = "log(Visitors on the first weekend)")

# Relation between the movie beeing a kids movie and visistors
plot(x = data_final$Kinderfilm, y = data_final$Besucher_wochenende1_log,
     main = "Relation between the movie beeing a kids movie and visistors", 
     ylab = "log(Visitors on the first weekend)", xlab = "Kids Movie",  names = c("Yes", "No"))
table(data_final$Kinderfilm)

# Relation between age rating and visistors
plot(x = data_final$FSK, y = data_final$Besucher_wochenende1_log,
     main = "Relation between age rating and visistors",
     ylab = "log(Visitors on the first weekend)", xlab = "Age rating")
table(data_final$FSK)

# Relation between genre and visistors
plot(x = data_final$Genre_Prognose, y = data_final$Besucher_wochenende1_log,
     las = 2, main = "Relation between genre and visistors",
     ylab = "log(Visitors on the first weekend)", xlab = "Genre")
table(data_final$Genre_Prognose)

# Relation between studio category and visistors
plot(x = data_final$Verleiherkategorie, y = data_final$Besucher_wochenende1_log,
     main = "Relation between studio category and visistors",
     ylab = "log(Visitors on the first weekend)", xlab = "Studio category")
table(data_final$Verleiherkategorie)

# Relation between premier month and visistors
plot(x = data_final$Monat, y = data_final$Besucher_wochenende1_log,
     main = "Relation between premier month and visistors", xlab = "Premier month",
     ylab = "log(Visitors on the first weekend)")

# Relation between seasonality and visistors
plot(x = data_final$Saison, y = data_final$Besucher_wochenende1_log,
     main = "Relation between seasonality and visistorse",
     ylab = "log(Visitors on the first weekend)", xlab = "Seasonality")
