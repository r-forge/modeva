AUC <-
function(obs = NULL, pred = NULL, model = NULL, interval = 0.01, 
                simplif = FALSE, plot = TRUE, plot.values = TRUE, 
                xlab = c("False positive rate", "(1-specificity)"), 
                ylab = c("True positive rate", "(sensitivity)"), 
                main = "ROC curve", ...) {
  # version 1.3 (4 Sep 2013)
  
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
  
  # next 3 lines: from Wintle et al 2005 supp mat "roc" function
  xy <- c(pred[obs == 0], pred[obs == 1])
  rnk <- rank(xy)
  AUC <- ((n0 * n1) + ((n0 * (n0 + 1))/2) - sum(rnk[1 : n0])) / (n0 * n1)
  
  if (simplif) {
    return(AUC)
  } else {  # end if simplif
    
    N <- length(obs)
    preval <- prevalence(obs)
    thresholds <- seq(0, 1, by = interval)
    Nthresh <- length(thresholds)
    true.pos <- true.neg <- sens <- spec <- false.pos.rate <- numeric(Nthresh)
    
    # next lines and plot based on a script by Regina Bispo (ISPA, Lisbon)
    # (R course NUFOR-UEvora 2009)
    for (i in 1 : Nthresh) {
      true.pos[i] <- sum(obs == 1 & pred >= thresholds[i])
      true.neg[i] <- sum(obs == 0 & pred < thresholds[i])
      sens[i] <- true.pos[i] / n1
      spec[i] <- true.neg[i] / n0
      false.pos.rate[i] <- 1 - spec[i]
    }  # end for i
    
    thresholds = data.frame(thresholds, true.pos, true.neg, 
                            sens, spec, false.pos.rate)
  }  # end if simplif else
  
  if (plot) {
    plot(c(1, false.pos.rate, 0), c(1, sens, 0), type = "o", pch = "", xlim = c(0, 1), ylim = c(0, 1), xlab = xlab, ylab = ylab, main = main, ...)
    abline(0, 1, col = "grey", lty = 2)
    if (plot.values) {
      text(1.0, 0.1, adj = 1, substitute(paste(AUC == a), list(a = round(AUC, 3))))
    }  # end if plot values
  }  # end if plot
  
  return (list(thresholds = thresholds, N = N, prevalence = preval, AUC = AUC))
  
}
