predPlot <- function(model = NULL, obs = NULL, pred = NULL, thresh = "preval", main = "Classified predicted values", legend.pos = "n", pch = 1, col = c("black", "grey")) {
  # version 1.4 (17 Apr 2022)
  
  obspred <- inputMunch(model, obs, pred, na.rm = FALSE)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  if (!(thresh == "preval" || (is.numeric(thresh) && thresh >= 0 && thresh <= 1))) stop ("'thresh' must be either 'preval' or a numeric value between 0 and 1.")
  if (thresh == "preval")  thresh <- prevalence(obs)

  pred0 <- pred[obs == 0]
  pred1 <- pred[obs == 1]
  
  # opar <- par(no.readonly = TRUE)
  # on.exit(par(opar))
  # par(mar = c(5, 5.2, 3, 1))
  par_mar <- par()$mar
  on.exit(par(mar = par_mar))
  par(mar = c(5, 5.2, 3, 1))
  
  plot(x = c(0, 1), y = c(-0.5, 1.5), xlab = "Predicted value", type = "n", ylab = "", yaxt = "n", main = main)
  axis(side = 2, at = c(0, 1), tick = FALSE, labels = c("Observed\nabsences", "Observed\npresences"), las = 1)
  abline(v = thresh, lty = 2)
  points(x = pred0, y = sapply(rep(0, length(pred0)), jitter, 10), pch = pch, col = ifelse(pred0 < thresh, col[2], col[1]))
  points(x = pred1, y = sapply(rep(1, length(pred1)), jitter, 10), pch = pch, col = ifelse(pred1 < thresh, col[2], col[1]))
  
  if (!is.na(legend.pos) && legend.pos != "n")  legend(legend.pos, legend = c("Predicted presence", "Predicted absence"), pch = pch, col = col)
}
