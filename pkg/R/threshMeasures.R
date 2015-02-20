threshMeasures <-
function(obs = NULL, pred = NULL, model = NULL, thresh = 0.5,
         measures = modEvAmethods("threshMeasures"), simplif = FALSE,
         plot = TRUE, plot.ordered = FALSE, standardize = TRUE,
         messages = TRUE, ...) {
  # version 2.5 (20 Jan 2013)
  # obs: a vector of observed presences (1) and absences (0) or another binary response variable
  # pred: a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike
  # model: instead of (and overriding) obs and pred, you can provide a model object of class "glm"
  # thresh: threshold value to separate predicted presences from predicted absences in 'pred'; may be "preval" or any real number between 0 and 1
  # measures: character vector of the evaluation measures to use
  # plot: logical, whether or not to produce a barplot of the calculated measures
  # plot.ordered: logical, whether to plot the measures in decreasing order rather than in input order
  # standardize: logical, whether to change measures that may range between -1 and +1 (namely kappa and TSS) to their corresponding value in the 0-to-1 scale, so thet they can compare directly to other measures
  # ...: arguments to be passed to the barplot function (if plot = TRUE)

  if(is.null(model)) {
    if (is.null(obs) | is.null(pred)) stop("You must provide either the 'obs'
and 'pred' vectors, or a 'model' object of class 'glm'")
  }  # end if null model
  else {
    if (!all(class(model) %in% c("glm", "lm"))) stop("'model' must be a
model object of class 'glm'")
    if (messages) {
      if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
      if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    }
    obs <- model$y
    pred <- model$fitted.values
  }  # end if !null model

  if (NA %in% obs | NA %in% pred)
    stop("Please remove (rows with) NA from your data")
  if (length(obs) != length(pred))
    stop("'obs' and 'pred' must have the same number of values
         (and in the same order)")
  if (!all(obs %in% c(0, 1)))
    stop ("'obs' must consist of binary observations of 0 or 1")
  #if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")
  if (!(thresh == "preval" | is.numeric(thresh)))
    stop("'thresh' must be either 'preval' or a numeric value between 0 and 1")
  if (thresh == "preval")  thresh <- prevalence(obs)

  obs0 <- obs == 0
  obs1 <- obs == 1
  pred0 <- pred < thresh
  pred1 <- pred >= thresh
  a <- sum(obs1 & pred1)
  b <- sum(obs0 & pred1)
  c <- sum(obs1 & pred0)
  d <- sum(obs0 & pred0)
  N <- a + b + c + d

  Nmeasures <- length(measures)
  measureValues <- as.vector(rep(NA, Nmeasures), mode = "numeric")

  for (i in 1:Nmeasures) {
    if (measures[i] == "AUC") measureValues[i] <- AUC(obs = obs, pred = pred,
                                                      simplif = TRUE)
    else if (measures[i] %in% modEvAmethods("threshMeasures")) {
      measureValues[i] <- evaluate(a, b, c, d, N, measure = measures[i])
      if (standardize == TRUE  &  measures[i] %in% c("TSS", "kappa")) {
        measureValues[i] <- standard01(measureValues[i])
        measures[i] <- paste("s", measures[i], sep = "")
        message("\n", measures[i], " standardized to the 0-1 scale for direct comparability
with other measures (type ?standard01 for more info);
use 'standardize = FALSE' if this is not what you wish")
      }  # end if standardize
    }  # end if measures[i] in modEvAmethods("threshMeasures")
    else {
      warning("'", measures[i], "' is not a valid measure;
type modEvAmethods('threshMeasures') for available options.")
      next
    }  # end else
  }  # end for i

  Measures <- matrix(data = measureValues, nrow = Nmeasures, ncol = 1,
                     dimnames = list(measures, "Value"))
  if (simplif) {  # shorter version for use with e.g. optiThresh function
    return(Measures)
  } else {
    prev <- prevalence(obs)
    conf.matrix <- matrix(c(a, b, c, d), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("pred1", "pred0"), c("obs1", "obs0")))
    if (plot) {
      names(measureValues) <- measures
      measures.plot <- measureValues
      if (plot.ordered) {
        measures.plot <- sort(measures.plot, decreasing = TRUE)
      }
      barplot(measures.plot, las = 3, ...)
    }  # end if plot
    return(list(N = N, Prevalence = prev, Threshold = thresh,
                ConfusionMatrix = conf.matrix, ThreshMeasures = Measures))
  }  # end else
}
