getBins <-
function(obs = NULL, pred = NULL, model = NULL, id = NULL, bin.method = "quantiles", n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15, min.prob.interval = 0.1, simplif = FALSE) {
  
  # version 3.4 (11 Sep 2014)
  # obs: a vector of observed presences (1) and absences (0) or another binary response variable
  # pred: a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike
  # model: instead of (and overriding) obs and pred, you can provide a model object of class "glm"
  # id: optional vector of row identifiers; must be of the same length and in the same order of 'obs' and 'pred' (or of the cases used to build 'model')
  
  if (!is.null(model)) {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model
  
  stopifnot(
    length(obs) == length(pred),
    !(NA %in% obs),
    !(NA %in% pred),
    obs %in% c(0, 1),
    pred >= 0,
    pred <= 1,
    is.null(id) | length(id) == length(pred),
    n.bins >= 2,
    min.bin.size >= 0,
    min.prob.interval > 0,
    min.prob.interval < 1
  )
  
  if(!(bin.method %in% modEvAmethods("getBins"))) stop("Invalid bin.method; type modEvAmethods('getBins') for available options.")
  
  N <- length(obs)
  
  # now get prob.bin (probability bin to which each point belongs):
  if (bin.method == "round.prob") {  # flaw: assymmetric (up to 11 bins, the first and last with half the prob range)
    message("Arguments n.bins and min.bin.size are ignored by this bin.method")
    prob.bin <- round(pred, digits = nchar(min.prob.interval) - 2)
  }  # end method round.prob
  
  if (bin.method == "prob.bins") {  # flaw: may generate empty or very small bins
    message("Arguments n.bins and min.bin.size are ignored by this bin.method")
    bin.cuts <- seq(from = 0, to = 1, by = min.prob.interval)
    prob.bin <- findInterval(pred, bin.cuts)
  }  # end method prob.bins
  
  if (bin.method == "size.bins") {
    message("Arguments n.bins and min.prob.interval are ignored by this bin.method")
    bin.method <- "n.bins"
    n.bins <- floor(N / min.bin.size)
    fixed.bin.size <- TRUE
  }  # end method size.bins
  
  if (bin.method == "n.bins") {   # note: bins do not have constant size (and some get less than min size)
    message("Argument min.prob.interval is ignored by this bin.method")
    if (fixed.bin.size) {
      prob.bin <- findInterval(pred, quantile(pred, probs = seq(from = 0, to = 1, by = 1 / n.bins)))
    } else {
      prob.bin <- cut(pred, n.bins)
    }
  }  # end method n.bins
  
  if (bin.method == "quantiles") {
    # quantile binning based on 'hosmerlem' function by Peter D. M. Macdonald (http://www.stat.sc.edu/~hitchcock/diseaseoutbreakRexample704.txt)
    cutpoints <- quantile(pred, probs = seq(0, 1, 1/n.bins))
    prob.bin <- findInterval(pred, cutpoints)
  }
  
  prob.bins <- sort(unique(prob.bin))  # lists the probability bins in the model
  
  bins.table <- t(as.data.frame(unclass(table(obs,prob.bin))))
  bins.table <- data.frame(rowSums(bins.table), bins.table[ , c(2, 1)])
  colnames(bins.table) <- c("N","N.pres","N.abs")
  bins.table$prevalence <- with(bins.table, N.pres / N)
  bins.table$mean.prob <- tapply(pred, prob.bin, mean)
  bins.table$median.prob <- tapply(pred, prob.bin, median)
  bins.table$difference <- with(bins.table, mean.prob - prevalence)
  bins.table$max.poss.diff <- ifelse(bins.table$mean.prob < 0.5, 1 - bins.table$mean.prob, bins.table$mean.prob)
  bins.table$adj.diff <- with(bins.table, abs(difference - max.poss.diff))
  bins.table$overpred <- apply(bins.table[ ,c("prevalence", "mean.prob")], 1, max)
  bins.table$underpred <- apply(bins.table[ ,c("prevalence", "mean.prob")], 1, min)
  
  bins.table <- bins.table[bins.table$N > 0, ]  # eliminates empty bins (which impede calculations)
  
  if(min(as.data.frame(bins.table)$N) < 15) warning("There is at least one bin with less than 15 values, for which comparisons may not be meaningful; consider using a bin.method that allows defining a minimum bin size")
  n.bins <- nrow(bins.table)
  
  return(list(prob.bin = prob.bin, bins.table = bins.table, N = N, n.bins = n.bins))
  
}
