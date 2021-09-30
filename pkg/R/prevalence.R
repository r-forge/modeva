prevalence <-
function (obs = NULL, model = NULL, event = 1, na.rm = TRUE) {
  # version 1.1 (30 sep 2021)

  #model_classes_implemented <- c("glm", "gam", "gbm", "randomForest", "bart")
  #if (any(class(obs) %in% model_classes_implemented))  model <- obs  # (in case the user provides unnamed model argument)
  
  if (!is.null(obs)) {
    if (!is(obs, "vector")) stop("'obs' must be a vector.")
    if (!(event %in% obs)) stop("'event' is not among of the values of 'obs'.")
  }
  
  if (!is.null(model)) {
    if (!is.null(obs)) warning("Argument 'obs' ignored in favour of 'model'.")
    if (is(model, "glm") || is(model, "gam")) {
      obs <- model$y
    } else if (is(model, "gbm")) {
      obs <- model$data$y
    } else if (is(model, "randomForest")) {
      obs <- model$y
    } else if (is(model, "bart")) {
      if (is.null(model$fit$data)) stop("'$fit$data' section not available in 'model'. Try computing the 'bart' model with 'keeptrees=TRUE', or providing 'obs' here instead of 'model'.")
      obs <- model$fit$data@y  # requires model ran with keeptrees=TRUE
    } else stop("'model' is of a non-implemented class.")
  }  # end if model
  
  if(na.rm) obs <- obs[!is.na(obs)]
  sum(obs == event) / length(obs)
}
