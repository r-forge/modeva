HLfit <-
function (model = NULL, obs = NULL, pred = NULL, bin.method = "quantiles", n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15, min.prob.interval = 0.1, simplif = FALSE, alpha = 0.05, plot = TRUE, plot.values = TRUE, plot.bin.size = TRUE, xlab = "Predicted probability", ylab = "Observed prevalence", ...) {
  # version 1.3 (19 July 2013)

  if (!is.null(model)) {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model

  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0, 1),
    pred >= 0,
    pred <= 1
  )

  bins <- getBins(obs = obs, pred = pred, bin.method = bin.method, n.bins = n.bins, fixed.bin.size = fixed.bin.size, min.bin.size = min.bin.size, min.prob.interval = min.prob.interval)
  n.bins <- nrow(bins$bins.table)

  # next 4 lines: adapted from hosmerlem function in http://www.stat.sc.edu/~hitchcock/diseaseoutbreakRexample704.txt
  observed <- xtabs(cbind(1 - obs, obs) ~ bins$prob.bin)
  expected <- xtabs(cbind(1 - pred, pred) ~ bins$prob.bin)
  chi.sq <- sum((observed - expected) ^ 2 / expected)
  p.value <- 1 - pchisq(chi.sq, df = n.bins - 2)

  if (simplif) return(list(chi.sq = chi.sq, p.value = p.value))

  # plotting loosely based on calibration.plot function in package PresenceAbsence
  if (plot) {
    N.total <- tapply(obs, bins$prob.bin, length)  # N cases in each bin
    N.presence <- tapply(obs, bins$prob.bin, sum)  # N presences in each bin
    Empty <- is.na(N.total)
    N.total[is.na(N.total)] <- 0
    N.presence[is.na(N.presence)] <- 0
    OBS.proportion <- N.presence / N.total
    OBS.proportion[Empty] <- NA
    df1.low <- 2 * (N.total - N.presence + 1)
    df2.low <- 2 * N.presence
    df1.up <- 2 * (N.presence + 1)
    df2.up <- 2 * (N.total - N.presence)
    N.bins <- length(unique(bins$prob.bin))  # fui eue
    Lower <- rep(0, N.bins)
    Upper <- rep(1, N.bins)
    TF <- N.presence != 0
    Lower[TF] <- N.presence[TF]/(N.presence[TF] + ((N.total[TF] - N.presence[TF] + 1) * qf(1 - alpha/2, df1.low[TF], df2.low[TF])))
    Lower[Empty] <- NA
    TF <- N.presence < N.total
    Upper[TF] <- ((N.presence[TF] + 1) * qf(1 - alpha/2, df1.up[TF], df2.up[TF]))/(N.total[TF] - N.presence[TF] + ((N.presence[TF] + 1) * qf(1 - alpha/2, df1.up[TF], df2.up[TF])))
    Upper[Empty] <- NA
    plot(c(-0.05, 1.05), c(-0.05, 1.05), type = "n", xlab = xlab, ylab = ylab, ...)
    abline(a = 0, b = 1, lty = 2)
    bin.centers <- bins$bins.table$median.prob  # fui eue
    for (i in 1:N.bins) {
      lines(rep(bin.centers[i], 2), c(Lower[i], Upper[i]))
    }
    points(bin.centers, OBS.proportion, pch = 20)

    if (plot.bin.size)  text(bin.centers, Upper + 0.07, labels = N.total)

    if (plot.values) {
      text(1, 0.2, adj = 1, substitute(paste(italic(HL) == a), list(a = round(chi.sq, 1))))
      if (p.value < 0.001) {
        text(1, 0, adj = 1, substitute(paste(italic(p) < a), list(a = 0.001)))
      } else {
        text(1, 0, adj = 1, substitute(paste(italic(p) == a), list(a = round(p.value, 3))))
      }
    }  # end if plot values
  }

  BinPred <- tapply(pred, bins$prob.bin, mean)
  bins.table <- data.frame(BinCenter = bin.centers, NBin = N.total, BinObs = OBS.proportion, BinPred = BinPred, BinObsCIlower = Lower, BinObsCIupper = Upper)

  return(list(bins.table = bins.table, chi.sq = chi.sq, DF = n.bins - 2, p.value = p.value))
}
