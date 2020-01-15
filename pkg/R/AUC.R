AUC <- function(model = NULL, obs = NULL, pred = NULL, simplif = FALSE,
                interval = 0.01, FPR.limits = c(0, 1), curve = "ROC", 
                method = "rank", plot = TRUE, diag = TRUE, diag.col = "grey",
                diag.lty = 1, curve.col = "black", curve.lty = 1, 
                curve.lwd = 2, plot.values = TRUE, plot.digits = 3,
                plot.preds = FALSE, grid = FALSE, xlab = "auto", ylab = "auto",
                ...) {
  # version 2.0 (8 Jan 2020)
  
  if (all.equal(FPR.limits, c(0, 1)) != TRUE) stop ("Sorry, 'FPR.limits' not yet implemented. Please use default values.")
  
  if (length(obs) != length(pred))  stop ("'obs' and 'pred' must be of the same length (and in the same order).")
  
  if (!is.null(model)) {
    if(!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model
  
  # new (15 Sep 2015):
  dat <- data.frame(obs, pred)
  n.in <- nrow(dat)
  dat <- na.omit(dat)
  n.out <- nrow(dat)
  if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
  obs <- dat$obs
  pred <- dat$pred
  
  stopifnot(
    obs %in% c(0,1),
    #    pred >= 0,
    #    pred <= 1,
    interval > 0,
    interval < 1
  )
  
  n1 <- sum(obs == 1)
  n0 <- sum(obs == 0)
  
  # next 3 lines from Wintle et al 2005 supp mat "roc" function
  xy <- c(pred[obs == 0], pred[obs == 1])
  rnk <- rank(xy)
  AUC <- ((n0 * n1) + ((n0 * (n0 + 1))/2) - sum(rnk[1 : n0])) / (n0 * n1)
  
  if (curve == "ROC" && simplif && !plot) {
    return(AUC)
  }
  
  if (curve == "PR") {
    AUC <- NA
    plot.values <- FALSE
  }
  
  # UNDER CONSTRUCTION:
  #  if (method == "rank") {
  #    if (curve != "ROC") {
  #      message("'rank' method not applicable to the specified 'curve'; using 'trapezoid' instead.")
  #      method <- "trapezoid"
  #    }
  # next 3 lines from Wintle et al 2005 supp mat "roc" function
  #xy <- c(pred[obs == 0], pred[obs == 1])
  #rnk <- rank(xy)
  #AUC <- ((n0 * n1) + ((n0 * (n0 + 1))/2) - sum(rnk[1 : n0])) / (n0 * n1)
  #  }
  #  else stop("'rank' is the only method currently implemented.")
  
  if (any(pred < 0) | any(pred > 1)) warning("Some of your predicted values are outside the [0, 1] interval within which thresholds are calculated.")
  
  N <- length(obs)
  preval <- prevalence(obs)
  thresholds <- seq(0, 1, by = interval)
  Nthresh <- length(thresholds)
  true.positives <- true.negatives <- sensitivity <- specificity <- precision <- false.pos.rate <- n.preds <- prop.preds <- numeric(Nthresh)
  
  for (t in 1 : Nthresh) {
    true.positives[t] <- sum(obs == 1 & pred >= thresholds[t])
    true.negatives[t] <- sum(obs == 0 & pred < thresholds[t])
    sensitivity[t] <- true.positives[t] / n1
    specificity[t] <- true.negatives[t] / n0
    precision[t] <- true.positives[t] / sum(pred >= thresholds[t])
    false.pos.rate[t] <- 1 - specificity[t]
    n.preds[t] <- sum(round(pred, nchar(Nthresh) - 1) == thresholds[t])
    prop.preds[t] <- n.preds[t] / length(pred)
  }
  thresholds.df <- data.frame(thresholds, true.positives, true.negatives, sensitivity, specificity, precision, false.pos.rate, n.preds, prop.preds)
  rownames(thresholds.df) <- thresholds
  
  # UNDER CONSTRUCTION:
#    if (method == "trapezoid") {
      #AUC <- with(na.omit(thresholds.df[ , c("sensitivity", "precision")]), caTools::trapz(x = sensitivity, y = precision))
#      pr <- na.omit(thresholds.df[ , c("sensitivity", "precision")])
#      x <- pr$sensitivity
#      y <- pr$precision
#      # next 2 lines from caTools::trapz:
#      idx <- 2:length(x)
#      AUC <- as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2
#      # next line adapted from https://stackoverflow.com/a/22418496:
#      AUC <- sum(diff(pr$sensitivity) * (pr$precision[-1] + pr$precision[-length(pr$precision)]) / 2)
#    }
  #  if (method == "integrate") {
  
  #  }
  
  if (plot) {
    if (curve == "ROC") {
      xx <- false.pos.rate
      yy <- sensitivity
      if (xlab == "auto") xlab <- c("False positive rate", "(1-specificity)")
      if (ylab == "auto") ylab <- c("True positive rate", "(sensitivity)")
    }
    if (curve == "PR") {
      xx <- sensitivity
      yy <- precision
      if (xlab == "auto") xlab <- c("Recall", "(sensitivity)")
      if (ylab == "auto") ylab <- c("Precision", "(positive predictive value)")
    }
    
    d <- ifelse(diag, "l", "n")
    plot(x = c(0, 1), y = c(0, 1), type = d, xlab = xlab, ylab = ylab, col = diag.col, lty = diag.lty, ...)  # plots the 0.5 diagonal (or not if diag=FALSE)
    
    if (grid) abline(h = thresholds, v = thresholds, col = "lightgrey")
    
    lines(x = xx, y = yy, col = curve.col, lty = curve.lty, lwd = curve.lwd)
    
    if (plot.preds == TRUE) plot.preds <- c("curve", "bottom")  # for back-compatibility
    if ("bottom" %in% plot.preds) {
      points(x = thresholds, y = rep(0, Nthresh), cex = 100 * prop.preds, col = "darkgrey")  # 20 * sqrt(prop.preds)
    }
    if ("curve" %in% plot.preds) {
      points(x = xx, y = yy, cex = 100 * prop.preds, col = "darkgrey")
    }
    
    if (plot.values) {
      text(1, 0.4, adj = 1, substitute(paste(AUC == a), list(a = round(AUC, plot.digits))))
    }  # end if plot.values
    
  }  # end if plot
  
  if (simplif)  return (AUC)
  
  return (list(thresholds = thresholds.df, N = N, prevalence = preval, AUC = AUC, AUCratio = AUC / 0.5))
}
