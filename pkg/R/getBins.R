getBins <- function (model = NULL, obs = NULL, pred = NULL, id = NULL,
bin.method, n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15,
min.prob.interval = 0.1, simplif = FALSE)  {
  
  # version 2.2 (6 Jun 2016)

  if (!is.null(model)) {
    if(!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
      if (!is.null(obs))
        message("Argument 'obs' ignored in favour of 'model'.")
      if (!is.null(pred))
        message("Argument 'pred' ignored in favour of 'model'.")
      obs <- model$y
      pred <- model$fitted.values
    }
    
    stopifnot(length(obs) == length(pred), 
              !(NA %in% obs), 
              !(NA %in% pred), 
              obs %in% c(0, 1), 
              #pred >= 0, 
              #pred <= 1, 
              is.null(id) | length(id) == length(pred), 
              n.bins >= 2, 
              min.bin.size >= 0, 
              min.prob.interval > 0, 
              min.prob.interval < 1)
  
    if (!(bin.method %in% modEvAmethods("getBins")))
      stop("Invalid bin.method; type modEvAmethods('getBins') for available options.")
  
    N <- length(obs)
  
    if (bin.method == "round.prob") {
      message("Arguments n.bins and min.bin.size are ignored by this bin.method")
      prob.bin <- round(pred, digits = nchar(min.prob.interval) - 2)
    }
  
    else if (bin.method == "prob.bins") {
      message("Arguments n.bins and min.bin.size are ignored by this bin.method")
      bin.cuts <- seq(from = min(0, min(pred)), to = max(1, max(pred)), by = min.prob.interval)
      prob.bin <- findInterval(pred, bin.cuts)
    }
  
    else if (bin.method == "size.bins") {
      message("Arguments n.bins and min.prob.interval are ignored by this bin.method")
      bin.method <- "n.bins"
      n.bins <- floor(N/min.bin.size)
      fixed.bin.size <- TRUE
    }
  
    if (bin.method == "n.bins") {  # can't have 'else' here because of previous 'if'
      message("Argument min.prob.interval is ignored by this bin.method")
      if (fixed.bin.size) {
        #prob.bin <- findInterval(pred, quantile(pred, probs = seq(from = 0, to = 1, by = 1 / (n.bins - 1))))
        prob.bin <- cut(seq_along(pred), n.bins)
      }
      else {
        prob.bin <- cut(pred, n.bins)
      }
    }  # end if n.bins
    
    else if (bin.method == "quantiles") {
      cutpoints <- quantile(pred, probs = seq(0, 1, by = 1/n.bins))
      prob.bin <- findInterval(pred, cutpoints)
    }
  
    prob.bins <- sort(unique(prob.bin))
    bins.table <- t(as.data.frame(unclass(table(obs, prob.bin))))
    bins.table <- data.frame(rowSums(bins.table), bins.table[, c(2, 1)])
    colnames(bins.table) <- c("N", "N.pres", "N.abs")
  
    bins.table$prevalence <- with(bins.table, N.pres/N)
    bins.table$mean.prob <- tapply(pred, prob.bin, mean)
    bins.table$median.prob <- tapply(pred, prob.bin, median)
    bins.table$difference <- with(bins.table, mean.prob - prevalence)
    bins.table$max.poss.diff <- ifelse(bins.table$mean.prob < 0.5,
                                       1 - bins.table$mean.prob,
                                       bins.table$mean.prob)
    bins.table$adj.diff <- with(bins.table, abs(difference - max.poss.diff))
    bins.table$overpred <- apply(bins.table[, c("prevalence", "mean.prob")], 
                                 MARGIN = 1, max)
    bins.table$underpred <- apply(bins.table[, c("prevalence", "mean.prob")], 
                                  MARGIN = 1, min)
    bins.table <- bins.table[bins.table$N > 0, ]
  
    if (min(as.data.frame(bins.table)$N) < 15)
      warning("There is at least one bin with less than 15 values, for which comparisons may not be meaningful; consider using a bin.method that allows defining a minimum bin size")
  
    n.bins <- nrow(bins.table)
    list(prob.bin = prob.bin, bins.table = bins.table, N = N, n.bins = n.bins)
  }
