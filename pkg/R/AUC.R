AUC <- function(obs = NULL, pred = NULL, model = NULL, interval = 0.01, 
       simplif = FALSE, plot = TRUE, plot.values = TRUE, plot.preds = FALSE, 
       grid = FALSE, xlab = c("False positive rate", "(1-specificity)"), 
       ylab = c("True positive rate", "(sensitivity)"), main = "ROC curve", ...) {
  # version 1.5 (16 Oct 2014)
  
  if (!is.null(model)) {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model
  
  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0,1),
    pred >= 0,
    pred <= 1,
    interval > 0,
    interval < 1
  )
  
  n1 <- sum(obs == 1)
  n0 <- sum(obs == 0) 
  
  # next 3 lines from Wintle et al 2005 supp mat "roc" function
  xy <- c(pred[obs == 0], pred[obs == 1])
  rnk <- rank(xy)
  AUC <- ((n0 * n1) + ((n0 * (n0 + 1))/2) - sum(rnk[1 : n0])) / (n0 * n1)
  
  if (simplif)  return(AUC)
  
  N <- length(obs)
  preval <- prevalence(obs)
  thresholds <- seq(0, 1, by = interval)
  Nthresh <- length(thresholds)
  true.positives <- true.negatives <- sensitivity <- specificity <- false.pos.rate <- n.preds <- prop.preds <- numeric(Nthresh)
  
  for (t in 1 : Nthresh) {
    true.positives[t] <- sum(obs == 1 & pred >= thresholds[t])
    true.negatives[t] <- sum(obs == 0 & pred < thresholds[t])
    sensitivity[t] <- true.positives[t] / n1
    specificity[t] <- true.negatives[t] / n0
    false.pos.rate[t] <- 1 - specificity[t]
    n.preds[t] <- sum(round(pred, nchar(Nthresh) - 1) == thresholds[t])
    prop.preds[t] <- n.preds[t] / length(pred)
  }; rm(t)
  
  if (plot) {
    plot(x = c(0, 1), y = c(0, 1), type = "l", xlab = xlab, ylab = ylab, main = main, col = "grey", ...)  # plots the 0.5 diagonal

    if (grid) abline(h = thresholds, v = thresholds, col = "lightgrey")
  
    lines(x = false.pos.rate, y = sensitivity, lwd = 2)  # ROC curve
  
    if (plot.values) {
      text(1.0, 0.1, adj = 1, substitute(paste(AUC == a), list(a = round(AUC, 3))))
    }  # end if plot.values
    
    if (plot.preds) {
      points(x = false.pos.rate, y = sensitivity, cex = rev(20 * sqrt(prop.preds)), col = "red")  # but why do I have to rev?
      #points(x = thresholds, y = specificity, cex = sqrt(100 * prop.preds), col = "blue")
      #points(x = false.pos.rate, y = sensitivity, cex = sqrt(100 * prop.preds), col = "red")  # ROC curve
      points(x = thresholds, y = rep(0, Nthresh), cex = 20 * sqrt(prop.preds), col = "blue")
    }  # end if plot.preds
  }  # end if plot
  
  return (list(thresholds = data.frame(thresholds, true.positives, true.negatives, sensitivity, specificity, false.pos.rate, n.preds, prop.preds), N = N, prevalence = preval, AUC = AUC, AUCratio = AUC / 0.5))
}
