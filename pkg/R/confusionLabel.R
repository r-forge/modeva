confusionLabel <- function(model = NULL, obs = NULL, pred = NULL, thresh, verbosity = 2) {
  # version 1.2 (26 Nov 2021)
  
  if (is.null(model)) {
    if (is.null(obs) | is.null(pred)) stop ("You must provide either the 'obs' and 'pred' vectors, or a 'model' object.")
    
  } else { # end if null model
    
    #if (!all(class(model) %in% c("glm", "lm"))) stop ("'model' must be a model object of class 'glm'")
    #if (!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
    if (verbosity > 0) {
      if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
      if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    }
    # obs <- model$y
    # pred <- model$fitted.values
    obspred <- mod2obspred(model)
    obs <- obspred[ , "obs"]
    pred <- obspred[ , "pred"]
    
  }  # end if !null model
  
  if (length(obs) != length(pred))  stop ("'obs' and 'pred' must have the same number of values (and in the same order).")

  res <- rep("", length(obs))
  res[pred >= thresh & obs == 1] <- "TruePos"
  res[pred < thresh & obs == 0] <- "TrueNeg"
  res[pred >= thresh & obs == 0] <- "FalsePos"
  res[pred < thresh & obs == 1] <- "FalseNeg"
  res
}
