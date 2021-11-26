prevalence <-
function (obs = NULL, model = NULL, event = 1, na.rm = TRUE) {
  # version 1.3 (26 Nov 2021)

  #model_classes_implemented <- c("glm", "gam", "gbm", "randomForest", "bart")
  #if (any(class(obs) %in% model_classes_implemented))  model <- obs  # (in case the user provides unnamed model argument)
  
  if (!is.null(obs)) {
    if (!is(obs, "vector") && !is(obs, "factor")) stop("'obs' must be of class 'vector' or 'factor'.")
      if (!(event %in% obs)) warning("'event' is not among of the values of 'obs'.")
  }
    
  if (!is.null(model)) {
    if (!is.null(obs)) warning("Argument 'obs' ignored in favour of 'model'.")
    obs <- mod2obspred(model, obs.only = TRUE)[ , "obs"]
  }  # end if model
  
  if(na.rm) obs <- obs[!is.na(obs)]
  sum(obs == event) / length(obs)
}
