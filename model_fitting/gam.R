load("data/data_final/data_final.RData")
source("utils/prediction_utils.R")
require(mgcv)
require(itsadug)

# Model without Google Trends data
formula0 = Besucher_wochenende1_log ~ 1 + s(Kopien, bs = "ps", k = 30, m = c(2, 2)) + Saison + 
  Genre_Prognose * Verleiherkategorie
am_week0 = gam(formula = formula0, method = "ML", select = FALSE, data = data_final)
summary(am_week0)
am_week0$aic
plot(x = am_week0, pages = 1)
gam.check(am_week0)

# Quantile-Quantile-Plot
qqnorm(residuals(am_week0))
qqline(residuals(am_week0))

# Outlier- und impact analysis
cook0 = cooks.distance(am_week0)
names(cook0) = rownames(data_final)
head(sort(x = cook0, decreasing = TRUE), 10)
cook0 = names(head(sort(x = cook0, decreasing = TRUE), 10))
data_final[cook0, 2]
leverage0 = influence(am_week0)
names(leverage0) = rownames(data_final)
head(sort(x = leverage0, decreasing = TRUE), 10)
leverage0 = names(head(sort(x = leverage0, decreasing = TRUE), 10))
data_final[leverage0, 2]

# Models with Google Trends data
# Week1
formula1 = Besucher_wochenende1_log ~ 1 + te(Kopien, Woche1_transformiert, bs = "ps", k = c(11, 11), m = 2) +
  Woche4_transformiert + Saison + Woerter + Genre_Prognose * Verleiherkategorie
am_week1 = gam(formula = formula1, method = "ML", select = FALSE, data = data_final)
summary(am_week1)

am_week1$aic
pvisgam(x = am_week1,  view = c("Kopien", "Woche1_transformiert"), plot.type = "persp", theta = -45, phi = 15, 
        color = "terrain")
gam.check(am_week1)

qqnorm(residuals(am_week1))
qqline(residuals(am_week1))

cook1 = cooks.distance(am_week1)
names(cook1) = rownames(data_final)
head(sort(x = cook1, decreasing = TRUE), 10)
cook1 = names(head(sort(x = cook1, decreasing = TRUE), 10))
data_final[cook1, 2]
leverage1 = influence(am_week1)
names(leverage1) = rownames(data_final)
head(sort(x = leverage1, decreasing = TRUE), 10)
leverage1 = names(head(sort(x = leverage1, decreasing = TRUE), 10))
data_final[leverage1, 2]


# Week2
formula2 = Besucher_wochenende1_log ~ 1 + s(Kopien, bs = "ps", k = 30, m = c(2, 2)) +
  s(Woche2_transformiert, bs = "ps", k = 20, m = c(2, 2)) + Woche3_transformiert + Woche4_transformiert + Saison + 
  Woerter + Genre_Prognose * Verleiherkategorie
am_week2 = gam(formula = formula2, method = "ML", select = FALSE, data = data_final)
summary(am_week2)

am_week2$aic
plot(x = am_week2, pages = 1)
gam.check(am_week2)

qqnorm(residuals(am_week2))
qqline(residuals(am_week2))

cook2 = cooks.distance(am_week2)
names(cook2) = rownames(data_final)
head(sort(x = cook2, decreasing = TRUE), 10)
cook2 = names(head(sort(x = cook2, decreasing = TRUE), 10))
data_final[cook2, 2]
leverage2 = influence(am_week2)
names(leverage2) = rownames(data_final)
head(sort(x = leverage2, decreasing = TRUE), 10)
leverage2 = names(head(sort(x = leverage2, decreasing = TRUE), 10))
data_final[leverage2, 2]

# Week3
formula3 = Besucher_wochenende1_log ~ 1 +  s(Kopien, bs = "ps", k = 30, m = c(2, 2)) + 
  s(Woche3_transformiert, bs = "ps", k = 20, m = c(2, 2)) + Saison + Woerter + Genre_Prognose * Verleiherkategorie +
  Woche4_transformiert + Woche6_transformiert
am_week3 = gam(formula = formula3, method = "ML", select = FALSE, data = data_final)
summary(am_week3)
am_week3$aic
plot(x = am_week3, pages = 1)
gam.check(am_week3)

qqnorm(residuals(am_week3))
qqline(residuals(am_week3))

cook3 = cooks.distance(am_week3)
names(cook3) = rownames(data_final)
head(sort(x = cook3, decreasing = TRUE), 10)
cook3 = names(head(sort(x = cook3, decreasing = TRUE), 10))
data_final[cook3, 2]
leverage3 = influence(am_week3)
names(leverage3) = rownames(data_final)
head(sort(x = leverage3, decreasing = TRUE), 10)
leverage3 = names(head(sort(x = leverage3, decreasing = TRUE), 10))
data_final[leverage3, 2]

# Week4
formula4 = Besucher_wochenende1_log ~ 1 +  s(Kopien, bs = "ps", k = 30, m = c(2, 2)) + Woche4_transformiert +
  Woche6_transformiert + Saison + Woerter + Genre_Prognose * Verleiherkategorie
am_week4 = gam(formula = formula4, method = "ML", select = FALSE, data = data_final)
c
summary(am_week4)
am_week4$aic
plot(x = am_week4, pages = 1)
gam.check(am_week4)

qqnorm(residuals(am_week4))
qqline(residuals(am_week4))

cook4 = cooks.distance(am_week4)
names(cook4) = rownames(data_final)
head(sort(x = cook4, decreasing = TRUE), 10)
cook4 = names(head(sort(x = cook4, decreasing = TRUE), 10))
data_final[cook4, 2]
leverage4 = influence(am_week4)
names(leverage4) = rownames(data_final)
head(sort(x = leverage4, decreasing = TRUE), 10)
leverage4 = names(head(sort(x = leverage4, decreasing = TRUE), 10))
data_final[leverage4, 2]

# Week5
formula5 = Besucher_wochenende1_log ~ 1 + s(Kopien, bs = "ps", k = 30, m = c(2, 2)) + Woche5_transformiert +
  Woche6_transformiert + Saison + Woerter + Genre_Prognose * Verleiherkategorie
am_week5 = gam(formula = formula5, method = "ML", select = FALSE, data = data_final)
summary(am_week5)
am_week5$aic
plot(x = am_week5, pages = 1)
gam.check(am_week5)

qqnorm(residuals(am_week5))
qqline(residuals(am_week5))

cook5 = cooks.distance(am_week5)
names(cook5) = rownames(data_final)
head(sort(x = cook5, decreasing = TRUE), 10)
cook5 = names(head(sort(x = cook5, decreasing = TRUE), 10))
data_final[cook5, 2]
leverage5 = influence(am_week5)
names(leverage5) = rownames(data_final)
head(sort(x = leverage5, decreasing = TRUE), 10)
leverage5 = names(head(sort(x = leverage5, decreasing = TRUE), 10))
data_final[leverage5, 2]

# Week6
formula6 = Besucher_wochenende1_log ~ 1 + s(Kopien, bs = "ps", k = 30, m = c(2, 2)) +
  s(Woche6_transformiert, bs = "ps", k = 20, m = c(2, 2)) + Saison + Woerter + Genre_Prognose * Verleiherkategorie
am_week6 = gam(formula = formula6, method = "ML", select = FALSE, data = data_final)
summary(am_week6)
am_week6$aic
plot(x = am_week6, pages = 1)
gam.check(am_week6)

qqnorm(residuals(am_week6))
qqline(residuals(am_week6))

cook6 = cooks.distance(am_week6)
names(cook6) = rownames(data_final)
head(sort(x = cook6, decreasing = TRUE), 10)
cook6 = names(head(sort(x = cook6, decreasing = TRUE), 10))
data_final[cook6, 2]
leverage6 = influence(am_week6)
names(leverage6) = rownames(data_final)
head(sort(x = leverage6, decreasing = TRUE), 10)
leverage6 = names(head(sort(x = leverage6, decreasing = TRUE), 10))
data_final[leverage6, 2]
save.image("results/gam/GAM.RData")

