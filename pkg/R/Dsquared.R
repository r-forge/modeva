Dsquared <- function(model = NULL, 
                     obs = NULL, 
                     pred = NULL, 
                     family = NULL, # needed only when 'model' not provided
                     adjust = FALSE, 
                     npar = NULL, # needed only when 'model' not provided
                     na.rm = TRUE,
                     rm.dup = FALSE) {
  # version 1.8 (6 May 2022)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")

  } else { # if model not provided
    
    if (is.null(family)) stop ("Without the 'model' argument, you must specify one of two model family options: 'binomial' or 'poisson' (in quotes).")
    else if (!is.character(family)) stop ("Argument 'family' must be provided as character (i.e. in quotes: 'binomial' or 'poisson').")
    else if (length(family) != 1 | !(family %in% c("binomial", "poisson"))) stop ("'family' must be either 'binomial' or 'poisson' (in quotes).")
    
    pred[pred == 0] <- 2e-16  # avoid NaN in log below
    pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below
    
    if (family == "binomial") {
      if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("'binomial' family implies that 'obs' should be binary (with values 0 or 1) and 'pred' should be bounded between 0 and 1.")
      link <- log(pred / (1 - pred))  # logit
    }  # end if binomial

    else if (family == "poisson") {
      if (any(obs %%1 != 0)) stop ("'poisson' family implies that 'obs' should be integer.")
      link <- log(pred)
    }  # end if poisson
    
    model <- glm(obs ~ link, family = family)
  }  # end if model not provided

  D2 <- (model$null.deviance - model$deviance) / model$null.deviance

  if (adjust) {
    if (model.provided) {
      n <- length(model$y)
      #p <- length(model$coefficients)
      p <- attributes(logLik(model))$df
    } else {
      if (is.null(npar)) stop ("Without the 'model' argument, 'adjust=TRUE' requires specifying 'npar' (number of parameters) of the underlying model.")
      n <- length(na.omit(obs))
      p <- npar
    }  # end if model.provided else

    D2 <- 1 - ((n - 1) / (n - p)) * (1 - D2)
  }  # end if adjust

  return (D2)
}
