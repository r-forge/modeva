Dsquared <-
function(obs = NULL, pred = NULL, model = NULL, adjust = FALSE) {
  # version 1.2 (28 Oct 2014)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)
  
  if (model.provided) {
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }

  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0, 1),
    pred >= 0,
    pred <= 1)
  
  D2 <- (model$null.deviance - model$deviance) / model$null.deviance
  
  if (adjust) {
    if(!model.provided) return(message("Adjusted D-squared not calculated, as it requires a model object (with its number of parameters) rather than just 'obs' and 'pred' values."))
    n <- length(model$fitted.values)
    p <- length(model$coefficients)
    D2 <- 1 - ((n - 1) / (n - p)) * (1 - D2)
  }  # end if adj
  
  return (D2)
}
