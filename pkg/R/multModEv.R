multModEv <-
function(models = NULL, obs.data = NULL, pred.data = NULL, measures = modEvAmethods("multModEv"), thresh = "preval", standardize = FALSE, bin.method = "quantiles", quiet = TRUE) {
  # version 1.9 (26 May 2014)

#  if (Favourability == TRUE & thresh == "preval") {
#    thresh <- 0.5
#    message("Threshold automatically set to 0.5, which corresponds to prevalence when Favourability is used.")
#  }

  for (i in measures) {
    if (!(i %in% modEvAmethods("multModEv"))) stop(i, " is not a valid measure; type modEvAmethods('multModEv') for available options.")
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
      obs.data[ , m] <- models[[m]] $ y
      pred.data[ , m] <- models[[m]] $ fitted.values
      #if (Favourability) fav.data[ , m] <- Fav(obs = obs.data[ , m], pred = pred.data[ , m])
    }  # end for m
  }  # end if models

  #if (Favourability) pred.data <- fav.data

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
    cat("Using", thresh, "as the threshold value to calculate", paste(thresh.measures, collapse = ", "))  # doesn't quite work with 'message' or 'paste' or with a 'sep'
  }  # this needs to be oustide the 'for' loop

  for (i in 1:n.models) {

    message("Evaluating model ", i, "...")

    if ("Preval" %in% measures)
      results[i, "Preval"] <- prevalence(obs.data[ , i])

    if ("Evenness" %in% measures)
      results[i, "Evenness"] <- evenness(obs.data[ , i])

    if ("AUC" %in% measures)
      results[i, "AUC"] <- AUC(obs = obs.data[ , i], pred = pred.data[ , i], simplif = TRUE)

    if (any(measures %in% modEvAmethods("threshMeasures"))) {
      for (i in 1:n.models)  for (tm in thresh.measures) {
        results[i, tm] <- threshMeasures(obs = obs.data[ , i], pred = pred.data[ , i], measures = tm, thresh = thresh, standardize = standardize, simplif = TRUE)
      }  # end for thresh.measures
    }  # end if measures in modEvAmethods("threshMeasures")

    #      thresh.measures <- optiThresh(obs.data[ , i], pred.data[ , i], plot = FALSE, optimize = "each")

    if (any(measures %in% c("HL", "HLp")))
      HL <- HLfit(obs.data[ , i], pred.data[ , i], bin.method = bin.method, simplif = TRUE)
    if ("HL" %in% measures)
      results[i, "HL"] <- HL $ chi.sq
    if ("HLp" %in% measures)
      results[i, "HLp"] <- HL $ p.value

#    if (any(measures %in% c("MillerInt", "MillerSlope", "MillerTest")))
#      Miller <- MillerCalib(obs = obs.data[ , i], pred = pred.data[ , i], plot = FALSE)
#    if ("MillerInt" %in% measures)
#      results[i, "MillerInt"] <- Miller["a.intercept", ]
#    if ("MillerSlope" %in% measures)
#      results[i, "MillerSlope"] <- Miller["b.slope", ]
#    if ("MillerTest" %in% measures)
#      results[i, "MillerTest"] <- Miller["p.a0b1", ]

    #if (any(measures %in% c("ABCc", "rABCc", "unityRsq")))
    #  ABC <- ABCmodev(obs.data[ , i], pred.data[ , i], bin.method = bin.method, plot = FALSE, simplif = TRUE)
    #if ("ABCc" %in% measures)
    #  results[i, "ABCc"] <- ABC $ ABCc
    #if ("rABCc" %in% measures)
    #  results[i, "rABCc"] <- ABC $ rABCc
    #if ("unityRsq" %in% measures)
    #  results[i, "unityRsq"] <- ABC $ unityR2

  }  # end for i

  if(standardize) {
    #    colnames(results) <- sub(pattern = "kappa", replacement = "skappa", x = colnames(results))
    #    colnames(results) <- sub(pattern = "TSS", replacement = "sTSS", x = colnames(results))
    colnames(results)[colnames(results) == "kappa"] <- "skappa"
    colnames(results)[colnames(results) == "TSS"] <- "sTSS"
  }  # end if standardize

  message("Finished!")
  return(data.frame(results))
}
