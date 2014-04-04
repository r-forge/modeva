optiPair <-
function (obs = NULL, pred = NULL, model = NULL, 
          measures = c("Sensitivity", "Specificity"), 
          interval = 0.01, plot = TRUE, plot.sum = FALSE, 
          plot.diff = FALSE, ylim = NULL, ...) {
  # version 1.4 (24 Jul 2013)
  # obs: a vector of observed presences (1) and absences (0) or another binary response variable
  # pred: a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike
  # model: instead of (and overriding) obs and pred, a model object of class "glm"
  # measures: the pair of measures whose curves to plot, e.g. c("Sensitivity", "Specificity")
  # interval: the interval of thresholds at which to calculate sensitivity and specificity
  # plot: logical indicating whether or not to plot the measures
  # plot.sum and plot.diff will optionally plot the sum (+) and/or the difference (-) between both measures in the pair
  # ... : additional arguments to be passed to the "plot" function
  
  if(length(measures) != 2) stop ("'measures' must contain two elements.")
  
  if(is.null(model)) {
    if (is.null(obs) | is.null(pred)) stop("You must provide either the 'obs' 
and 'pred' vectors, or a 'model' object of class 'glm'")
  }  # end if null model
  else {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    model <- NULL  # so the message is not repeated for each threshold
  }  # end if model
  
  if (NA %in% obs | NA %in% pred) stop("Please remove (rows with) NA 
                                       from your data")
  if (length(obs) != length(pred)) stop("'obs' and 'pred' must have the same 
                                        number of values (and in the same order)")
  if (!all(obs %in% c(0, 1))) stop ("'obs' must consist of binary observations 
                                    of 0 or 1")
  if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")
  
  measures.values <- optiThresh(obs = obs, pred = pred, interval = interval, measures = measures, optimize = "each", simplif = TRUE)
  measures.values$Difference <- abs(measures.values[ , 1] - measures.values[ , 2])
  measures.values$Sum <- rowSums(measures.values[ , 1:2])
  measures.values$Mean <- rowMeans(measures.values[ , 1:2])
  measures.values$Threshold <- as.numeric(rownames(measures.values))
  
  MinDiff <- min(measures.values$Difference)
  MaxSum <- max(measures.values$Sum)
  MaxMean <- max(measures.values$Mean)
  
  ThreshDiff <- with(measures.values, Threshold[which.min(Difference)])
  ThreshSum <- with(measures.values, Threshold[which.max(Sum)])
  ThreshMean <- with(measures.values, Threshold[which.max(Mean)])
  
  if (plot) {
    
    finite <- as.matrix(measures.values[ , 1:2])
    finite <- finite[is.finite(finite)]
    if(is.null(ylim)) {
      if(plot.sum) ylim <- c(min(finite), MaxSum)
      else ylim <- c(min(finite), max(finite))
    }  # end if null ylim
    
    plot(measures.values[ , 1] ~ measures.values$Threshold, pch = 20, xlab = "Threshold", ylab = measures, ylim = ylim, ...)
    # title(ylab = measures, col.lab = c("black", "grey"))  # error: "col.lab" has the wrong length
    points(measures.values[ , 2] ~ measures.values$Threshold, pch = 20, col = "darkgrey")
    abline(h = measures.values[which.min(measures.values$Difference), 1], col = "lightgrey", lty = 2)
    abline(v = ThreshDiff, col = "lightgrey", lty = 2)
    
    if (plot.sum) {
      with(measures.values, points(Sum ~ Threshold, pch = '+'))
      abline(v = ThreshSum, col = "lightgrey", lty = 2)
      abline(h = MaxSum, col = "lightgrey", lty = 2)
    }    # end if plot sum
    
    if (plot.diff) {
      with(measures.values, points(Difference ~ Threshold, pch = '-', col = "grey"))
      abline(v = ThreshDiff, col = "grey", lty = 2)
      abline(h = MinDiff, col = "grey", lty = 2)
    }  # end if plot diff
  }  # end if plot
  
  return(list(measures.values = measures.values, MinDiff = MinDiff, ThreshDiff = ThreshDiff, MaxSum = MaxSum, ThreshSum = ThreshSum, MaxMean = MaxMean, ThreshMean = ThreshMean))
  
}
