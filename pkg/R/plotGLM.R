plotGLM <-
function(model = NULL, obs = NULL, pred = NULL, link = "logit",
         plot.values = FALSE, xlab = "Logit (Y)",
         ylab = "Predicted probability", main = "Model plot", ...) {
  # version 1.8 (28 Sep 2015)
  # obs: presence/absence or other binary (1-0) observed data
  # pred: values predicted by a GLM of the binary observed data
  # model: instead of (and overriding) obs & pred, you can provide a model object of class "glm"
  # link: link function of the GLM; only 'logit' is implemented
  # plot.values: logical, whether to report the values of deviance and adjusted deviance in the plot (requires the 'model' argument and the 'Dsquared' function)
  # ...: arguments to pass to the 'plot' function

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
  }  # end if model
  
  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0, 1),
    pred >= 0,
    pred <= 1)

  pred[pred == 0] <- 2e-16  # avoid log 0 below
  pred[pred == 1] <- 1 - 2e-16  # avoid division by 0 below
  
  logit <- log(pred / (1 - pred))
  if (!model.provided) model <- glm(obs ~ logit, family = "binomial")

  plot(pred ~ logit, ylim = c(0, 1), type = "n", xlab = xlab, ylab = ylab,
       main = main, ...)
  points(obs ~ logit, pch = 1, col = "darkgrey")
  abline(v = 0, lty = 5, col = "grey")  # y of P = 0.5
  #abline(h = pred[which.min(abs(logit))], col = "lightgrey", lty = 2)
  points(pred ~ logit, pch = 20, cex = 0.6)

  if (plot.values) {
    Dsq <- Dsquared(model = model, adjust = FALSE)
    if (max(logit) > abs(min(logit)))  x.loc <- c(max(logit), 1)
    else x.loc <- c(min(logit), 0)
    text(x = x.loc[1], y = 0.6, adj = x.loc[2], labels = substitute(paste(D^2 == a), list(a = round(Dsq, 3))))
    if (model.provided) {  # adjDsq needs n parameters in original model, not just our model created from obs~logit
      adjDsq <- Dsquared(model = model, adjust = TRUE)
      text(x = x.loc[1], y = 0.4, adj = x.loc[2], labels = substitute(paste('D'['adj']^2) == a, list(a = round(adjDsq, 3))))

    }
  }  # end if plot.val
}
