varImp <- function(model, reorder = TRUE, plot = TRUE, plot.type = "barplot", col = c("darkblue", "salmon"), ci = TRUE, ci.type = "sd", legend = TRUE, ...) {

  # version 1.2 (22 Dec 2022)

  # if 'col' has length 2 and varImp has negative values (e.g. for z-value), those will get the second colour

  if (is(model, "glm")) {
    ci <- FALSE

    #  if (measure == "z") {
    varimp <- summary(model)$coefficients[-1, "z value"]
    ylab <- "z value"
    #  }

    # if (measure == "Wald") {  # requires 'fuzzySim' and 'aod'
    #   ylab <- "Wald"
    #   legend <- FALSE
    #   smry <- summaryWald(model, interceptLast = FALSE)[-1, ]
    #   varimp <- smry[ , "Wald"]
    #   names(varimp) <- rownames(smry)
    #   varimp <- varimp[order(varimp, decreasing = TRUE)]
    # }
  }  # end if glm

  else if (is(model, "gbm")) {
    ci <- FALSE
    ylab <- "Relative influence"
    smry <- summary(model, plotit = FALSE)
    varimp <- smry[ , "rel.inf"] / 100
    names(varimp) <- smry[ , "var"]
  }

  else if (is(model, "randomForest")) {
    ci <- FALSE
    varimp <- model$importance  # / nrow(model$importance) / 100  # doesn't work well for mean accuracy decrease
    names(varimp) <- rownames(model$importance)
    ylab <- colnames(model$importance)
  }

  else if (is(model, "bart")) {
    ylab <- "Proportion of splits"
    varimps <- model$varcount / rowSums(model$varcount)
    varimp <- colMeans(varimps)
  }

  else stop ("'model' is of a non-implemented class.")

  if (reorder) {
    varimp <- varimp[order(abs(varimp), decreasing = TRUE)]
    if (is(model, "bart"))  varimps <- varimps[ , names(varimp)]
  }

  if (ci) {
    if (ci.type == "range") {
      ci_lower <- apply(varimps, 2, min)
      ci_upper <- apply(varimps, 2, max)
    } else if (ci.type == "sd") {
      vsd <- sapply(as.data.frame(varimps), sd)
      ci_lower <- varimp - vsd
      ci_upper <- varimp + vsd
    }
  }  # end if ci

  if (plot) {
    if (length(col) == 1) col <- rep(col, 2)

    ymax <- ifelse(ci, max(varimps), max(varimp))

    space <- 0.25  # space between bars in barplot

    barplot(abs(varimp),
            space = space,
            col = ifelse(varimp > 0, col[1], col[2]),
            names = names(varimp),
            ylab = ylab,
            ylim = c(0, ymax),
            las = 2,
            ...)

    if (ci) {
      nbars <- length(names(varimp))
      xbars <- ((1 : nbars) - space) + space * (0 : (nbars - 1))
      arrows(x0 = xbars, x1 = xbars, y0 = ci_lower, y1 = ci_upper, angle = 90, code = 3, length = 0.03)
    }

    if (legend && any(varimp < 0)) legend("topright", legend = c("positive", "negative"), fill = c(col[1], col[2]), bty = "n")
  }

  return(varimp)
}
