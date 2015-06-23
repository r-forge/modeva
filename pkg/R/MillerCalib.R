MillerCalib <- function(model = NULL, obs = NULL, pred = NULL, plot = TRUE, plot.values = TRUE, digits = 4, xlab = "", ylab = "", main = "Miller calibration", ...) {
  # version 1.2 (3 Nov 2014)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
  }

  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0,1),
    pred >= 0,
    pred <= 1
  )

  logit <- log(pred / (1 - pred))
  mod <- glm(obs ~ logit, family = binomial)
  intercept <- mod$coef[[1]]
  slope <- mod$coef[[2]]
  std.err <- summary(mod)$coefficients["logit", "Std. Error"]
  slope.p <- abs((slope - 1) / sqrt(std.err^2 + 0))  # Paternoster 98; http://stats.stackexchange.com/questions/55501/test-a-significant-difference-between-two-slope-values

  if (plot) {
    ymin <- min(0, intercept)
    ymax <- max(1, intercept + 0.1)
    plot(c(0, 1), c(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main)
    abline(0, 1, col = "lightgrey", lty = 2)
    abline(intercept, slope)
    if (plot.values) {
      plotext <- paste("intercept =" , round(intercept, digits), "\nslope =", round(slope, digits), "\nslope p-value =", round(slope.p, digits))
      text(x = 0, y = ymax - 0.25, adj = 0, labels = plotext)
    }
  }
  return(list(intercept = intercept, slope = slope, slope.pvalue = slope.p))
}  # end MillerCalib function
