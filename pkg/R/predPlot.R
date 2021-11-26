predPlot <- function(model = NULL, obs = NULL, pred = NULL, thresh = "preval", main = "Classified predicted values", legend.pos = "n", pch = 1, col = c("black", "grey")) {
  # version 1.2 (26 Nov 2021)
  
  if (!is.null(model)) {
    #if (!("glm" %in% class(model)) || family(model)$family != "binomial") stop("'model' must be of class 'glm' and family 'binomial'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    # obs <- model$y
    # pred <- model$fitted.values
    obspred <- mod2obspred(model)
    obs <- obspred[ , "obs"]
    pred <- obspred[ , "pred"]
  } else {
    if (is.null(obs) || is.null(pred)) stop ("You must provide either 'model' or a combination of 'obs' and 'pred'.")
    if (!is.numeric(obs) || !is.numeric(pred)) stop ("'obs' and 'pred' must be numeric.")
    if (length(obs) != length(pred)) stop("'obs' and 'pred' must have the same length.")
  }
  
  if (!(thresh == "preval" || (is.numeric(thresh) && thresh >= 0 && thresh <= 1))) stop ("'thresh' must be either 'preval' or a numeric value between 0 and 1.")
  if (thresh == "preval")  thresh <- prevalence(obs)

  pred0 <- pred[obs == 0]
  pred1 <- pred[obs == 1]
  
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mar = c(5, 5.2, 3, 1))

  plot(x = c(0, 1), y = c(-0.5, 1.5), xlab = "Predicted value", type = "n", ylab = "", yaxt = "n", main = main)
  axis(side = 2, at = c(0, 1), tick = FALSE, labels = c("Observed\nabsences", "Observed\npresences"), las = 1)
  abline(v = thresh, lty = 2)
  points(x = pred0, y = sapply(rep(0, length(pred0)), jitter, 10), pch = pch, col = ifelse(pred0 < thresh, col[2], col[1]))
  points(x = pred1, y = sapply(rep(1, length(pred1)), jitter, 10), pch = pch, col = ifelse(pred1 < thresh, col[2], col[1]))
  
  if (!is.na(legend.pos) && legend.pos != "n")  legend(legend.pos, legend = c("Predicted presence", "Predicted absence"), pch = pch, col = col)
}
