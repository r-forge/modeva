multModEv <-
function(models = NULL, obs.data = NULL, pred.data = NULL, measures = modEvAmethods("multModEv"), standardize = FALSE, thresh = 0.5, bin.method = "quantiles", quiet = TRUE) {
  # version 2.0 (24 Jun 2015)

#  if (Favourability == TRUE & thresh == "preval") {
#    thresh <- 0.5
#    message("Threshold automatically set to 0.5, which corresponds to prevalence when Favourability is used.")
#  }

  for (m in measures) {
    if (!(m %in% modEvAmethods("multModEv"))) stop(m, " is not a valid measure; type modEvAmethods('multModEv') for available options.")
  }

  if (!bin.method %in% modEvAmethods("getBins")) stop("Invalid bin.method; type modEvAmethods('getBins') for available options.")

  if (is.null(models)) {
    if (is.null(obs.data) | is.null(pred.data)) stop("You must provide either a list of model object(s) of class 'glm', or a set of obs.data + pred.data with matching dimensions.")
    #if (Favourability) message("'pred.data' converted with the Fav function; set Favourability to FALSE if this is not what you wish.")
  }  # end if !models

  else {
    if (!is.null(obs.data)) message("Argument 'obs.data' ignored in favour of 'models'.")
    if (!is.null(pred.data)) message("Argument 'pred.data' ignored in favour of 'models'.")

    n <- sapply(models, function(x) length(resid(x)))
    if(!(all(n == n[1]))) stop("Models must all be based on the same sample size.")

    obs.data <- pred.data <- fav.data <- matrix(data = NA, nrow = length(resid(models[[1]])), ncol = length(models), dimnames = list(NULL, names(models)))
    for (m in 1 : length(models)) {
      obs.data[ , m] <- models[[m]]$y
      pred.data[ , m] <- models[[m]]$fitted.values
      #if (Favourability) fav.data[ , m] <- Fav(obs = obs.data[ , m], pred = pred.data[ , m])
    }  # end for m
  }  # end if models

  # if (Favourability) pred.data <- fav.data

#  stopifnot(
#    is.null(models) | is.list(models),
#    dim(obs.data) == dim(pred.data),
#    as.vector(as.matrix(obs.data)) %in% c(0,1),
#    as.vector(as.matrix(pred.data)) >= 0,
#    as.vector(as.matrix(pred.data)) <= 1,
#    is.logical(Favourability),
#  )

  n.models <- ncol(obs.data)
  n.measures <- length(measures)
  results <- matrix(NA, nrow = n.models, ncol = n.measures)
  rownames(results) <- colnames(obs.data)
  colnames(results) <- measures

  message(n.models, " models to evaluate; ", n.measures,  " measures to calculate for each.")

  if (any(measures %in% modEvAmethods("threshMeasures"))) {
    thresh.measures <- measures[measures %in% modEvAmethods("threshMeasures")]
    cat("- using '", thresh, "' as the threshold value to calculate ", paste(thresh.measures, collapse = ", "), "\n", sep = "")  # doesn't quite work with 'message' or 'paste' or with a 'sep'
  }  # this needs to be oustide the 'for' loop

  bin.measures <- c("HL", "HL.p", "RMSE")  # ABC, unityRsq
  if (any(measures %in% bin.measures)) {
    bin.measures <- measures[measures %in% bin.measures]
    cat("- using '", bin.method, "' as the bin method to calculate ", paste(bin.measures, collapse = ", "), "\n", sep = "")  # doesn't quite work with 'message' or 'paste' or with a 'sep'
  }  # this needs to be oustide the 'for' loop

for (m in 1:n.models) {

    message("Evaluating model ", m, "...")

    if ("Prevalence" %in% measures)
      results[m, "Prevalence"] <- prevalence(obs.data[ , m])

    if ("Evenness" %in% measures)
      results[m, "Evenness"] <- evenness(obs.data[ , m])

    if ("AUC" %in% measures)
      results[m, "AUC"] <- AUC(obs = obs.data[ , m], pred = pred.data[ , m], simplif = TRUE)

    if (any(measures %in% modEvAmethods("threshMeasures"))) {
      for (m in 1:n.models)  for (tm in thresh.measures) {
        results[m, tm] <- threshMeasures(obs = obs.data[ , m], pred = pred.data[ , m], measures = tm, thresh = thresh, standardize = standardize, simplif = TRUE)
      }; rm(m, tm)
    }  # end if measures in modEvAmethods("threshMeasures")
    # thresh.measures <- optiThresh(obs = obs.data[ , m], pred = pred.data[ , m], plot = FALSE, optimize = "each")

    if (any(measures %in% c("HL", "HL.p"))) {
      for (m in 1:n.models) {
        HL <- HLfit(obs = obs.data[ , m], pred = pred.data[ , m], bin.method = bin.method, simplif = TRUE)
        if ("HL" %in% measures)  results[m, "HL"] <- HL$chi.sq
        if ("HL.p" %in% measures)  results[m, "HL.p"] <- HL$p.value
        if ("RMSE" %in% measures)  results[m, "RMSE"] <- HL$RMSE
      }; rm(m)
    }  # end if HL
    
    if (any(measures %in% c("Miller.int", "Miller.slope", "Miller.p"))) {
      for (m in 1:n.models) {
        Miller <- MillerCalib(obs = obs.data[ , m], pred = pred.data[ , m], plot = FALSE)
        if ("Miller.int" %in% measures)
          results[m, "Miller.int"] <- Miller$intercept
        if ("Miller.slope" %in% measures)
          results[m, "Miller.slope"] <- Miller$slope
        if ("Miller.p" %in% measures)
          results[m, "Miller.p"] <- Miller$slope.pvalue
      }; rm(m)
    } # end if Miller

    #if (any(measures %in% c("ABCc", "rABCc", "unityRsq")))
    #  ABC <- ABCmodev(obs.data[ , m], pred.data[ , m], bin.method = bin.method, plot = FALSE, simplif = TRUE)
    #if ("ABCc" %in% measures)
    #  results[m, "ABCc"] <- ABC $ ABCc
    #if ("rABCc" %in% measures)
    #  results[m, "rABCc"] <- ABC $ rABCc
    #if ("unityRsq" %in% measures)
    #  results[m, "unityRsq"] <- ABC $ unityR2

  }  # end for m

  if (standardize) {
    #    colnames(results) <- sub(pattern = "kappa", replacement = "skappa", x = colnames(results))
    #    colnames(results) <- sub(pattern = "TSS", replacement = "sTSS", x = colnames(results))
    colnames(results)[colnames(results) == "kappa"] <- "skappa"
    colnames(results)[colnames(results) == "TSS"] <- "sTSS"
  }  # end if standardize

  results <- data.frame(Model = rownames(results), results, row.names = NULL)
  message("Finished!")
  return(results)
}

