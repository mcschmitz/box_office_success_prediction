require(gtrendsR)
source("utils/plotting_utils.R")
load("data/ankers.RData")
load("data/data_final/data_final.RData")

# Google Trends example output
gdata = gtrends(keyword = c("Star Wars", "The Hobbit", "Hangover", "The Hunger Games", "James Bond"),
                geo = "DE", time = "2011-11-01 2016-08-01")$interest_over_time
colnames(gdata) = c("Date", "Volume", "Geo", "TimeWindow", "SearchTerm", "gprop", "category")
dates = seq(as.POSIXct("2011-01-01"), as.POSIXct("2016-08-01"), by = "6 mon")
gdata$Volume[gdata$Volume == "<1"] = 0
gdata$Volume = as.numeric(gdata$Volume)
plot = ggplot(aes(y = Volume, x = Date, color = SearchTerm, group = SearchTerm), data = gdata) + geom_line(size = 1) + 
  scale_x_datetime(breaks = dates, labels = strftime(dates, format = "%m/%y")) + stat_con + 
  scale_color_hue(h.start = 270) + theme(legend.position = c(0.2, 0.7), legend.title = element_blank())
par(bg = "white")
png("readme_plots/trends_example.png", width = 1200, height = 400, res = 120)
plot
dev.off()

# Scaled anchor terms
example1 = gtrends(keyword = c("Hamburg", "Frankfurt", "Spiegel", "Braunschweig", "Siegen"), 
                   geo = "DE", time = "2011-11-01 2016-08-01")$interest_over_time
example2 = gtrends(keyword = c("Hamburg", "Plauen", "Bad Hersfeld", "Reinbek", "Nordkurier"),
                    geo = "DE", time = "2011-11-01 2016-08-01")$interest_over_time
example3 = gtrends(keyword = c("Hamburg", "Allgäuer Zeitung", "Fränkische Nachrichten", "Schleswiger Nachrichten"),
                    geo = "DE", time = "2011-11-01 2016-08-01")$interest_over_time
ex_hamburg = rbind(example1, example2[-c(1:248), ], example3[-c(1:248), ])[, c(1, 2, 5)]
colnames(ex_hamburg) = c("Date", "Volume", "Anchor")
ex_hamburg$Volume[ex_hamburg$Volume == "<1"] = 0
ex_hamburg$Volume = as.numeric(ex_hamburg$Volume)
ex_hamburg$Anchor = rep(ankers, each = 248)
ex_hamburg$Anchor = as.factor(ex_hamburg$Anchor)
for(i in 1:(length(ankers) - 1)) {
  value = try(gtrends(keyword = c(ankers[i], ankers[i + 1]),
                      geo = "DE", time = "2011-11-01 2016-08-01")$interest_over_time)
  anker1 = paste0(tolower(ankers[i]), collapse = "")
  anker2 = paste0(tolower(ankers[i + 1]), collapse = "")
  assign(paste0("gt_", anker1,"_", anker2), value = value)
}

l = nrow(gt_hamburg_frankfurt)
gt_hamburg_frankfurt = gt_hamburg_frankfurt$hits[249:l]
gt_hamburg_spiegel = max(gt_hamburg_frankfurt) * gt_frankfurt_spiegel$hits[249:l]/100
gt_hamburg_braunschweig = max(gt_hamburg_spiegel) * gt_spiegel_braunschweig$hits[249:l]/100
gt_hamburg_siegen = max(gt_hamburg_braunschweig) * gt_braunschweig_siegen$hits[249:l]/100
gt_hamburg_plauen = max(gt_hamburg_siegen) * gt_siegen_plauen$hits[249:l]/100
gt_hamburg_badhersfeld = max(gt_hamburg_plauen) * `gt_plauen_bad hersfeld`$hits[249:l]/100
gt_hamburg_reinbek = max(gt_hamburg_badhersfeld) * `gt_bad hersfeld_reinbek`$hits[249:l]/100
gt_hamburg_nordkurier = max(gt_hamburg_reinbek) * gt_reinbek_nordkurier$hits[249:l]/100
gt_hamburg_allgaeuerzeitung = max(gt_hamburg_nordkurier) * `gt_nordkurier_allgäuer zeitung`$hits[249:l]/100
gt_hamburg_fraenkischenachrichten = max(gt_hamburg_allgaeuerzeitung) * 
  `gt_allgäuer zeitung_fränkische nachrichten`$hits[l]/100
gt_hamburg_schleswigernachrichten = max(gt_hamburg_fraenkischenachrichten) * 
  `gt_fränkische nachrichten_schleswiger nachrichten`$hits[249:l]/100

ex2 = data.frame(gt_hamburg_frankfurt, gt_hamburg_spiegel, gt_hamburg_braunschweig, gt_hamburg_siegen, 
                 gt_hamburg_plauen, gt_hamburg_badhersfeld, gt_hamburg_reinbek, gt_hamburg_nordkurier, 
                 gt_hamburg_allgaeuerzeitung, gt_hamburg_fraenkischenachrichten, gt_hamburg_schleswigernachrichten)
ex2$Hamburg = ex_hamburg$Volume[1:248]
ex2$Datum = ex_hamburg$Date[1:248]
ex2 = ex2[, c(13, 12, 1:11)]
colnames(ex2) = c("Date", ankers)
ex2 = melt(ex2, "Date")
colnames(ex2) = c("Date", "Anchor", "Volume")
ex_hamburg$Scaled = "Unscaled"
ex2$Scaled = "Scaled"
ex2 = rbind(ex_hamburg, ex2)
ex2$Scaled = as.factor(ex2$Scaled)
ex2$Scaled = relevel(ex2$Scaled, "Unscaled")
rownames(ex2) = 1:nrow(ex2)
cols = colorRampPalette(brewer.pal(9, "Set1"))(12)
dates = seq(as.POSIXct("2011-1-1"), as.POSIXct("2017-1-1"), by = "12 mon")
plot1 = ggplot(data = ex2, mapping = aes(x = Date, y = Volume, alpha = Scaled, color = Anchor)) + 
  geom_line(size = 1) + stat_con +  
  theme(legend.position = c(0.15, 0.98), legend.background = element_rect(fill = "transparent")) + 
  scale_alpha_discrete(name = "", range = c(0.4, 1),guide = guide_legend(reverse = FALSE, title = NULL)) + 
  scale_colour_manual(values = cols, guide = FALSE) + 
  scale_x_datetime(breaks = dates,  labels = strftime(dates, format = "%m/%y"))

plot2 = ggplot(data = ex2, mapping = aes(x = Date, y = Volume, alpha = Scaled, color = Anchor)) + 
  scale_alpha_discrete(name = "", range = c(0.4, 1), guide = guide_legend(reverse = FALSE, title = NULL)) + 
  geom_line(size = 1) + stat_con + ylim(c(0, 9.6)) + theme(legend.position = "none") +
  scale_colour_manual(values = cols, guide = FALSE) + ylab("") +
  scale_x_datetime(breaks = dates,  labels = strftime(dates, format = "%m/%y"))

par(bg = "white")
png("readme_plots/anchors.png", width = 1200, height = 400, res = 120)
plot_grid(plot1, plot2)
dev.off()


# Ambiguous movie titles
mama = data_final[75, ]
goethe = data_final[210, ]
example_median1 = gtrends(keyword = "Mama", geo = "DE", time = "2011-11-01 2015-01-01")$interest_over_time
example_median2 = gtrends(keyword = "Fack Ju Göhte", geo = "DE", time = "2011-11-01 2015-01-01")$interest_over_time
example_median1$hits[example_median1$hits == "<1"] = 0
example_median2$hits[example_median2$hits == "<1"] = 0
example_median1$hits = as.numeric(example_median1$hits)
example_median2$hits = as.numeric(example_median2$hits)

plot1 = ggplot(data = example_median1, mapping = aes(x = date, y = hits)) + 
  geom_line(size = 1) + ylim (0, 100) + stat_con + 
  scale_x_datetime(breaks = dates, labels = strftime(dates, format = "%m/%y")) + labs(x = "Date", y = "Search Volume") +
  geom_segment(x = as.numeric(mama$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(mama$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               y = 18, yend = 100, linetype = 2, color = "blue3") + 
  geom_segment(x = as.numeric(mama$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(mama$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               y = 18, yend = 100, linetype = 2, color = "blue3") + 
  geom_segment(x = as.numeric(mama$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(mama$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               y = 10, yend = 0, linetype = 2, color = "blue3") +
  geom_segment(x = as.numeric(mama$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(mama$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               y = 10, yend = 0, linetype = 2, color = "blue3") +
  geom_text(x = as.numeric(mama$Prognosedatum1 + as.difftime(tim = 10, units = "weeks")),
            y = 15, label = "Forecast period", size = 5, color = "blue3")

plot2 = ggplot(data = example_median2, aes(x = date, y = hits)) + 
  geom_line(size = 1) + ylim (0, 100) + stat_con + 
  scale_x_datetime(breaks = dates, labels = strftime(dates, format = "%m/%y")) + 
  labs(x = "Datum", y = "Suchanfragen") + 
  geom_segment(x = as.numeric(goethe$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(goethe$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               y = 68, yend = 100, linetype = 2, color = "blue3") + 
  geom_segment(x = as.numeric(goethe$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(goethe$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               y = 68, yend = 100, linetype = 2, color = "blue3") + 
  geom_segment(x = as.numeric(goethe$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(goethe$Prognosedatum1 - as.difftime(tim = 1, units = "weeks")),
               y = 60, yend = 0, linetype = 2, color = "blue3") +
  geom_segment(x = as.numeric(goethe$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               xend =  as.numeric(goethe$Prognosedatum6 - as.difftime(tim = 1, units = "weeks")),
               y = 60, yend = 0, linetype = 2, color = "blue3") +
  geom_text(x = as.numeric(goethe$Prognosedatum3 - as.difftime(tim = 1, units = "weeks")),
            y = 65, label = "Forecast period", size = 5, color = "blue3")


par(bg = "white")
png("readme_plots/median_normalization.png", width = 1200, height = 400, res = 120)
plot_grid(plot1, plot2)
dev.off()
