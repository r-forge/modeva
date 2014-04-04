plotGLM <-
function(obs = NULL, pred = NULL, model = NULL, link = "logit", 
                    plot.values = FALSE, xlab = "Logit (y)", 
                    ylab = "Predicted probability", main = "Model plot", ...) {
  # version 1.6 (8 Nov 2013)
  # obs: presence/absence or other binary (1-0) observed data
  # pred: values predicted by a GLM of the binary observed data
  # model: instead of (and overriding) obs & pred, you can provide a model object of class "glm"
  # link: link function of the GLM; only 'logit' is implemented
  # plot.values: logical, whether to report the values of deviance and adjusted deviance in the plot (requires the 'model' argument and the 'Dsquared' function)
  # ...: arguments to pass to the 'plot' function
  
  if (!is.null(model)) {
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model
  
  if ((is.null(obs) | is.null(pred)) & is.null(model)) stop("You must provide 
either 'obs' and 'pred', or a 'model' object of class 'glm'")
  
  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0, 1),
    pred >= 0,
    pred <= 1)
  
  if (link == "logit") {
    y <- log(pred / (1 - pred))
  } else {
    stop("only 'logit' link is implemented")
  }
  
  plot(pred ~ y, ylim = c(0, 1), type = "n", xlab = xlab, ylab = ylab, 
       main = main, ...)
  points(obs ~ y, pch = 1, col = "darkgrey")
  abline(v = 0, lty = 5, col = "grey")  # y of P = 0.5
  #abline(h = pred[which.min(abs(y))], col = "lightgrey", lty = 2)
  points(pred ~ y, pch = 20, cex = 0.6)
  
  if (plot.values) {
    if (is.null(model)) {
      plot.values <- FALSE
      message("'plot.values' automatically converted to FALSE because 
'model' (necessary to calculate those values) was not provided")
    }  # end if is.null model
  }  # end if plot.val I
  
  if (plot.values) {
    Dsq <- Dsquared(model, adjust = FALSE)
    adjDsq <- Dsquared(model, adjust = TRUE)
    if (max(y) > abs(min(y)))  loc <- c(max(y), 1)
    else loc <- c(min(y), 0)
    text(x = loc[1], y = 0.6, adj = loc[2], 
         labels = substitute(paste(D^2 == a), list(a = round(Dsq, 3))))
    text(x = loc[1], y = 0.4, adj = loc[2], 
         labels = substitute(paste(adj.D^2 == a), list(a = round(adjDsq, 3))))
  }  # end if plot.val II
}
