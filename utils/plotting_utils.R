require(ggplot2)
require(reshape2)
require(gridExtra)
require(RColorBrewer)

plot_requests <- function(x, alpha, names, ...){
  #' Function to plot the weekly development of the average value of x over time
  #' 
  #' @param x [numeric]: Numeric value to plot over time
  #'
  #' @return Plots the weekly average of requests made
  
  x_mean <- apply(X = x, MARGIN = 1, FUN = mean , na.rm = TRUE)
  x_melt <- melt(x)
  names(x_melt) <- names
  
  ggplot(data = x_melt, mapping = aes(y = Req)) + 
    geom_line(colour = "brown4", aes(group = Movie, x = Date, alpha = alpha), size = 1) +
    labs(x = "Weeks till premiere", ...) +
    scale_x_discrete(labels = factor(6:1)) + scale_alpha("Visitors:", range = c(0,1),
                labels = thousand_dot) +
    stat_summary(mapping = aes(x = Date, group = 1, colour = "Avg."), size = 1.5, fun.y = "mean", geom = "line", 
                 data = x_melt) + scale_color_manual("", values = "black") + 
    theme(legend.position = c(0.22, 0.8)) + stat_con
}

thousand_dot <- function(x) {
  format(x, big.mark = " ", scientific = FALSE)
}

number_ticks <- function(n) {function(limits) pretty(limits, n)}

stat_con <- theme_classic() + 
  theme(text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 12),
        legend.text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text.y = element_text(size = 12), legend.text.align = 0, strip.placement = "outside", 
        strip.background = element_blank(), axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
