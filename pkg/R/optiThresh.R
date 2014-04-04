optiThresh <-
function(obs = NULL, pred = NULL, model = NULL, interval = 0.01, 
         measures = modEvAmethods("threshMeasures"), 
         optimize = modEvAmethods("optiThresh"), simplif = FALSE, 
         plot = TRUE, sep.plots = FALSE, xlab = "Threshold", ...) {
  # version 2.6 (20 Jan 2013)
  # obs: a vector of observed presences (1) and absences (0) or another binary response variable
  # pred: a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike
  # model: instead of (and overriding) obs and pred, you can provide a model object of class "glm"
  # optimize: the threshold optimization criteria to use; "each" optimizes the threshold for each model evaluation measure, while the remaining options optimize all measures according to the specified criterion
  # interval: the interval of thresholds at which to calculate evaluation measures
  # measures: model evaluation measures for which to calculate the optimal thresholds
  # plot: logical indicating whether or not to plot the values of each evaluation measure at all thresholds
  # sep.plots: logical. If TRUE, each plot is presented separately (you need to be recording R plot history to be able to browse through them all); if FALSE (the default), all plots are presented together in the same window
  # ...: additional arguments to be passed to the "plot" function
  # Note: some measures cannot be calculated for thresholds at which there are zeros in the confusion matrix, hence the eventual 'NaN' or 'Inf' in results. Also, optimization may be deceiving for some measures; use plot = TRUE and inspect the plot(s).
  
  if (!is.null(model)) {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    model <- NULL  # so the message is not repeated for each threshold
  }  # end if model
  
  input.measures <- measures
  
  if ("minSensSpecDiff" %in% optimize | "maxSensSpecSum" %in% optimize) {
    if (!("Sensitivity" %in% measures)) {
      measures <- c(measures, "Sensitivity")
    }  # end if !Sensitivity
    if (!("Specificity" %in% measures)) {
      measures <- c(measures, "Specificity")
    }  # end if !Specificity
  }  # end if minSensSpecDiff
  
  if("maxKappa" %in% optimize & !("kappa" %in% measures)) {
    measures <- c(measures, "kappa")
  }
  
  if("maxTSS" %in% optimize & !("TSS" %in% measures)) {
    measures <- c(measures, "TSS")
  }
  
  thresholds <- seq(from = 0, to = 1, by = interval)
  Nthresholds <- length(thresholds)
  Nmeasures <- length(measures)
  all.thresholds <- data.frame(matrix(data = NA, nrow = Nthresholds, 
                                      ncol = Nmeasures), row.names = thresholds)
  colnames(all.thresholds) = measures
  
  for (t in 1 : Nthresholds) for (m in 1 : Nmeasures) {
    all.thresholds[t, m] <- threshMeasures(obs = obs, pred = pred, 
                                           thresh = thresholds[t], 
                                           measures = measures[m], 
                                           standardize = FALSE, simplif = TRUE)
  }  # end for t for m
  
  if (simplif) {  # shorter version for use with e.g. the optiPair function
    return(all.thresholds)
  }  # end if simplif
  else {
    results <- list(all.thresholds = all.thresholds)  # start a list of results
    
    input.optimize <- optimize
    
    if (plot == TRUE & !("each" %in% optimize)) optimize <- c("each", optimize)
    
    if ("each" %in% optimize) {
      optimals.each <- data.frame(matrix(data = NA, nrow = Nmeasures, ncol = 4))
      colnames(optimals.each) <- c("measure", "threshold", "value", "type")
      optimals.each[1] <- measures
      goodness.measures <- c("CCR", "Sensitivity", "Specificity", "PPP", "NPP", 
                             "kappa", "TSS", "NMI", "OddsRatio")
      badness.measures <- c("Omission", "Commission", "Misclass", "UPR", "OPR")
      change.measures <- c("PPI", "PAI")
      
      for (m in 1 : Nmeasures) {
        if (measures[m] %in% (goodness.measures)) {  # optimal is maximum
          optimals.each[m, "threshold"] <- as.numeric(
            rownames(all.thresholds)[which.max(all.thresholds[ , m])])
          optimals.each[m, "value"] <- max(all.thresholds[ , m], na.rm = TRUE)
          optimals.each[m, "type"] <- "maximum"
        }  # end if measure in goodness
        else {
          if (measures[m] %in% (badness.measures)) {  # optimal is minimum
            optimals.each[m, "threshold"] <- as.numeric(rownames(all.thresholds)[which.min(all.thresholds[, m])])
            optimals.each[m, "value"] <- min(all.thresholds[ ,m], na.rm = TRUE)
            optimals.each[m, "type"] <- "minimum"
          }  # end if measure in badness
          else {
            if (measures[m] %in% (change.measures)) {  # optimal is closest to zero
              optimals.each[m, "threshold"] <- as.numeric(
                rownames(all.thresholds)[which.min(abs(all.thresholds[ , m]))])
              optimals.each[m, "value"] <- min(abs(all.thresholds[ , m]), 
                                               na.rm = TRUE)
              optimals.each[m, "type"] <- "closest to zero"
            }  # end if measure in change
          }  # end 2nd else
        }  # end 1st else
      }  # end for m
      if ("each" %in% input.optimize)  results <- c(results, 
                        optimals.each = list(optimals.each))  # add this to results
    }  # end if each
    
    criteria <- optimize[optimize != "each"]
    Ncriteria <- length(criteria)
    
    if (Ncriteria > 0) {
      
      optimals.criteria <- data.frame(matrix(data = NA, nrow = Nmeasures, 
                                             ncol = Ncriteria))
      rownames(optimals.criteria) <- measures
      colnames(optimals.criteria) <- criteria
      
      if ("preval" %in% criteria) {
        for (m in 1 : Nmeasures) {
          optimals.criteria[m, "preval"] <- threshMeasures(
            obs = obs, pred = pred, thresh = "preval", measures = measures[m], 
            standardize = FALSE, simplif = TRUE)
        }
      }  # end if preval
      
      if ("minSensSpecDiff" %in% criteria) {
        all.thresholds$SensSpecDiff <- with(all.thresholds, 
                                            abs(Sensitivity - Specificity))
        minSensSpecDiff <- thresholds[which.min(all.thresholds$SensSpecDiff)]
        for (m in 1:Nmeasures) {
          optimals.criteria[m, "minSensSpecDiff"] <- threshMeasures(
            obs = obs, pred = pred, thresh = minSensSpecDiff, 
            measures = measures[m], standardize = FALSE, simplif = TRUE)
        }
      }
      
      if ("maxSensSpecSum" %in% criteria) {
        all.thresholds$SensSpecSum <- with(all.thresholds, 
                                           Sensitivity + Specificity)
        maxSensSpecSum <- thresholds[which.max(all.thresholds$SensSpecSum)]
        for (m in 1 : Nmeasures) {
          optimals.criteria[m, "maxSensSpecSum"] <- threshMeasures(
            obs = obs, pred = pred, thresh = maxSensSpecSum, 
            measures = measures[m], standardize = FALSE, simplif = TRUE)
        }
      }
      
      if ("maxKappa" %in% criteria) {
        if (!("kappa" %in% measures)) {
          for (t in 1 : Nthresholds) {
            all.thresholds$kappa <- threshMeasures(
              obs = obs, pred = pred, thresh = thresholds[t], 
              measures = "kappa", standardize = FALSE, simplif = TRUE)
          }
        }
        maxKappa <- thresholds[which.max(all.thresholds$kappa)]
        for (m in 1 : Nmeasures) {
          optimals.criteria[m, "maxKappa"] <- threshMeasures(
            obs = obs, pred = pred, thresh = maxKappa, measures = measures[m], 
            standardize = FALSE, simplif = TRUE)
        }
      }
      
      if ("maxTSS" %in% criteria) {
        if (!("TSS" %in% measures)) {
          for (t in 1 : Nthresholds) {
            all.thresholds$TSS <- threshMeasures(
              obs = obs, pred = pred, thresh = thresholds[t], measures = "TSS", 
              standardize = FALSE, simplif = TRUE)
          }
        }
        maxTSS <- thresholds[which.max(all.thresholds$TSS)]
        for (m in 1 : Nmeasures) {
          optimals.criteria[m, "maxTSS"] <- threshMeasures(
            obs = obs, pred = pred, thresh = maxTSS, measures = measures[m], 
            standardize = FALSE, simplif = TRUE)
        }
      }
      
      if ("0.5" %in% criteria) {
        for (m in 1 : Nmeasures) {
          optimals.criteria[m,"0.5"] <- all.thresholds[rownames(
            all.thresholds) == 0.5, m]
        }
      }
      
      results <- c(results, optimals.criteria = list(optimals.criteria))  # add this to results
    }  # end if Ncriteria > 0
    
    if (plot) {
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      n.input.measures <- length(input.measures)
      if (sep.plots) {
        par(mfrow = c(1,1))
      } else {
        if (n.input.measures > 4)  par(mar = c(1, 4.5, 1, 1))
        par(mfrow = arrangePlots(n.input.measures))
      }  # end if sep.plots else
      for (m in 1 : n.input.measures) {
        plot(all.thresholds[ , m] ~ thresholds, ylab = input.measures[m], ...)
        if ("each" %in% input.optimize) {
          abline(v = optimals.each[m, "threshold"], col = "grey", lty = 2)  # vertical line on optimal threshold
          abline(h = optimals.each[m, "value"], col = "grey", lty = 2)  # horiz line on optimal value
        }
      }  # end for m
    }  # end if plot
    
    return(results)
    
  }  # end if simplif else
}
