RsqGLM <- function(obs = NULL, pred = NULL, model = NULL) {
  # version 1.2 (3 Jan 2015)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values

  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("Sorry, 'obs' and 'pred' options currently only implemented for binomial GLMs (binary response variable with values 0 or 1) with logit link.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }

  #stopifnot(
  #  family(model)$family == "binomial",
  #  family(model)$link == "logit"
  #)

  null.mod <- glm(obs ~ 1, family = family(model))
  loglike.M <- as.numeric(logLik(model))
  loglike.0 <- as.numeric(logLik(null.mod))
  #like.M <- exp(loglike.M)
  #like.0 <- exp(loglike.0)
  #like.M <- like.M + 2.2e-16
  #like.0 <- like.0 + 2.2e-16  # zeros were causing problems, but this too
  N <- length(obs)

  # from http://stackoverflow.com/questions/3337044/model-fit-statistics-for-a-logistic-regression/15949904#15949904:
  #CoxSnell <- 1-exp((model$deviance - model$null.deviance) / 2 * N)
  #Nagelkerke <- CoxSnell / (1 - exp((- model$null.deviance) / N))
  #McFadden <- 1 - ((model$deviance / -2)/(model$null.deviance / -2))

  # based on Nagelkerke 1991:
  CoxSnell <- 1 - exp(-(2 / N) * (loglike.M - loglike.0))
  Nagelkerke <- CoxSnell / (1 - exp((2 * N ^ (-1)) * loglike.0))

  # based on Allison 2014:
  #CoxSnell <- 1 - (like.0 / like.M) ^ (2 / N)
  #Nagelkerke <- CoxSnell / (1 - like.0 ^ (2 / N))
  McFadden <- 1 - (loglike.M / loglike.0)
  Tjur <- mean(pred[obs == 1]) - mean(pred[obs == 0])
  #Tjur <- abs(diff(unique(ave(pred, obs))))
  #Tjur <- diff(tapply(pred, obs, mean))
  sqPearson <- cor(obs, pred) ^ 2

  return(list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke, McFadden = McFadden, Tjur = Tjur, sqPearson = sqPearson))
}