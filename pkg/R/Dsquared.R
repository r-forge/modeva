Dsquared <-
function(model, adjust = FALSE) {
  # version 1.1 (13 Aug 2013)
  d2 <- (model$null.deviance - model$deviance) / model$null.deviance
  if (adjust) {
    n <- length(model$fitted.values)
    p <- length(model$coefficients)
    d2 <- 1 - ((n - 1) / (n - p)) * (1 - d2)
  }
  return (d2)
}
