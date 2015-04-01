Dsquared <-
function(model = NULL, obs = NULL, pred = NULL, adjust = FALSE) {
  # version 1.3 (3 Jan 2015)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values

  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("Sorry, 'obs' and 'pred' options currently only implemented for binomial GLMs (binary response variable with values 0 or 1) with logit link.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }

  D2 <- (model$null.deviance - model$deviance) / model$null.deviance

  if (adjust) {
    if (!model.provided) return(message("Adjusted D-squared not calculated, as it requires a model object (with its number of parameters) rather than just 'obs' and 'pred' values."))

    n <- length(model$fitted.values)
    #p <- length(model$coefficients)
    p <- attributes(logLik(model))$df
    D2 <- 1 - ((n - 1) / (n - p)) * (1 - D2)
  }  # end if adj

  return (D2)
}
