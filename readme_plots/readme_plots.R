require(gtrendsR)
source("utils/plotting_utils.R")

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
