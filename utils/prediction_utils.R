calculate_interval <- function(gam, prediction, range) {
  #' Function to calculate a forecast intervall for generalized additive models
  #' 
  #' @param gam [gam]: generalized additive model
  #' @param prediction [list]: list with two elements. fit: point forecast, se.fit: standard deviation for the forecast
  #' @param range [nuumeric]: niveau of the forecast intervall. Should be betwenn [0, 1].
  #' 
  #' @return list with upper and lower bound of the forecast intervall

  fit <- prediction$fit
  sd_error <- prediction$se.fit
  scale <- gam$scale
  df_residuals <- gam$df.residual

  model_error <- sqrt(scale + sd_error ^ 2)
  quantile <- qt(p = 1 - (1 - range) / 2, df = df_residuals)

  lower <- exp(fit - quantile * model_error)
  upper <- exp(fit + quantile * model_error)

    return(list("lower" = lower, "upper" = upper))
}


