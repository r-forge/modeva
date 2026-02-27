poMeasures <- function(model = NULL, obs = NULL, pred = NULL, n.bins = 100, bin.width = "default", method = "spearman", rm.dup.classes = NA, rm.dup.points = FALSE, plot = TRUE, plot.lines = TRUE, plot.values = TRUE, cex.values = 1, plot.digits = 3, main = c("Moving-binned predictions", "Unbinned predictions"), na.rm = TRUE, verbosity = 2, ...) {
  
  # version 1.0 (27 Feb 2026)
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup.points, pbg = FALSE, verbosity = verbosity)
  if ("obs" %in% names(obspred)) obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  # pred_in <- pred
  
  if (!is.null(obs)) pred <- pred[obs == 1]
  if (length(pred) == 0) {
    warning("No presences available. Returning NULL.")
    return(NULL)
  }
  
  if (inherits(pred, "SpatRaster")) {
    if (inherits(obs, "data.frame") || inherits(obs, "matrix") || inherits(obs, "SpatVector")) {
      pred <- terra::extract(pred, obs)
    } else {
      stop("When 'pred' is a 'SpatRaster', 'obs' must be either a 'SpatVector' of the presence points or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively.")
    }
  }
  
  bins <- getBins(pred = pred, n.bins = n.bins, bin.width = bin.width, 
                  bin.method = "mov.bins") $ bins.table
  
  bins <- bins[ , c("min", "max", "N", "mean.prob", "median.prob")]
  
  bins_noNA <- na.omit(bins[ , c("N", "mean.prob")])
  Nbins <- nrow(bins_noNA)
  
  bPOGI <- stats::cor.test(bins$N, bins$mean.prob, method = method, 
                         use = "pairwise.complete.obs")
  
  # POS <- trend::sens.slope(bins$N)  # replaced with:
  
  # pois_fit <- glm(bins$N ~ bins$mean.prob, family = poisson)
  # pois_smr <- summary(pois_fit)
  # POS <- c(coef = pois_smr$coefficients["bins$mean.prob", "Estimate"],
  #          p = pois_smr$coefficients["bins$mean.prob", "Pr(>|z|)"])
  
  # Tau
  # POT <- cor.test(pred, seq_along(pred), method = "kendall",  use = "pairwise.complete.obs")
  # POT <- Kendall::MannKendall(pred)
  
  # bin-independent:
  
  # rand <- stats::runif(pred)
  # dens_rand <- density(rand, from = 0, to = 1)
  dens_pred <- density(pred, from = 0, to = 1)
  fhat <- approx(dens_pred$x, dens_pred$y, xout = pred) $ y

  dPOGI = cor.test(pred, fhat, method = method)
  
  # DK <- ks.test(pred, rand, alternative = "greater")
  # DW <- wilcox.test(pred, rand, alternative = "greater")
  W <- stats::wilcox.test(pred, mu = 0.5, alternative = "greater", digits.rank = 7)
  # DS <- moments::agostino.test(pred, alternative = "greater")
  
  # normalize W using rank-biserial correlation (sample-size-independent magnitude):
  N <- length(pred)
  # Wstar <- as.numeric(W$statistic) / (N * (N + 1) / 2)
  # PODS <- 2 * Wstar - 1
  PODS <- effectsize::rank_biserial(W)
  
  
  if (plot) {
    
    # plotting parameters:
    par_mgp <- par()$mgp
    on.exit(par(mgp = par_mgp))
    par(mgp = c(1.8, 0.7, 0))  # values and labels closer to axis
    
    # bin-dependent plot:
    plot(bins$mean.prob, bins$N, 
         xlim = c(0, 1),
         main = main[1], 
         xlab = "Bin mean", ylab = "N observations", 
         col = "royalblue", pch = 20, cex = 0.3,
         cex.axis = cex.values, ...)
    lines(bins$mean.prob, bins$N, col = "steelblue4", lwd = 0.6)
    lines(bins_noNA$mean.prob, bins_noNA$N, col = "lightgrey", lwd = 0.5)
    
    # reference (null) line:
    # abline(h = length(pred_in) / nrow(bins), col = "darkgrey", lty = "dashed")
    
    # Sen's slope line (deprecated):
    # abline(a = median(bins$N - POS$estimates * bins$mean.prob, na.rm = TRUE), 
    #        b = POS$estimates, lty = "dashed", col = "darkturquoise")
    
    # Poisson regression line:
    # xseq <- seq(min(bins$mean.prob, na.rm = TRUE), max(bins$mean.prob, na.rm = TRUE), length.out = nrow(bins))
    # yhat <- predict(pois_fit, newdata = data.frame(bins.mean = xseq), type = "response")
    # lines(xseq, yhat, lty = "dashed", col = "darkturquoise")
    
    if (plot.values) {
      text(x = 0, y = max(bins$N, na.rm = TRUE), 
           labels = paste0("bPOGI = ", format(round(bPOGI$estimate, plot.digits), nsmall = 3), 
                           "\n(p = ", format(bPOGI$p.value, digits = plot.digits, scientific = TRUE), ")",
                           "\n\n",
                           "Nbins = ", Nbins
                           # "POT = ", format(round(POT$tau, plot.digits), nsmall = 3),
                           # "  (p = ", format(round(POT$sl, plot.digits), nsmall = 3), ")"
           ), 
           adj = c(0, 1), cex = cex.values)
    }  # end if plot vals
    
    
    # bin-independent plot:
    
    # dens_ylim <- range(c(dens_pred$y, dens_rand$y))
    dens_ylim <- range(dens_pred$y)
    
    # plot(dens_pred, col = "royalblue", lwd = 2,
    # plot(pred, fhat, 
    plot(dens_pred$x, dens_pred$y, 
         col = "royalblue", pch = 20, cex = 0.2, 
         xlim = c(0, 1), ylim = dens_ylim, #zero.line = FALSE,
         main = main[length(main)],
         xlab = "Predicted value", ylab = "Density",
         cex.axis = cex.values, ...
    )
    
    # reference lines:
    # lines(dens_rand, col = "darkgrey", lty = "dashed")
    abline(v = 0.5, col = "darkgrey", lty = "dashed")
    
    # Sen's slope (median pairwise slope):
    # slopes <- outer(fhat, fhat, "-") / outer(pred, pred, "-")
    # sen_slope <- median(slopes[upper.tri(slopes)], na.rm = TRUE)
    # # sen_slope <- trend::sens.slope(fhat)
    # abline(a = median(fhat - sen_slope * pred, na.rm = TRUE), 
    #        b = sen_slope, 
    #        lty = "dashed", col = "darkturquoise")
    
    if (plot.values) {
      text(x = 0, y = dens_ylim[2], 
           paste0("dPOGI = ", format(round(dPOGI$estimate, plot.digits), nsmall = 3), 
                  "\n(p = ", format(dPOGI$p.value, digits = plot.digits, scientific = TRUE), ")",
                  "\n\n",
                  "PODS = ", round(PODS$r_rank_biserial, plot.digits), " [", round(PODS$CI_low, plot.digits), ", ", round(PODS$CI_high, plot.digits), "]",
                  "\n(Wp = ", format(W$p.value, digits = plot.digits, scientific = TRUE), ")",
                  # "\n",
                  # "DS = ", round(DS$statistic["skew"], plot.digits)
                  "\n\n",
                  "N = ", N 
           ),
           adj = c(0, 1), cex = cex.values)
    }  # end if plot vals
  }  # end if plot
  
  return(list(bins = bins, Nbins = Nbins, bPOGI = bPOGI, dPOGI = dPOGI, W = W, PODS = PODS, N = N))
}
