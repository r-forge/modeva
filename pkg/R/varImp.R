varImp <- function(model, imp.type = "each", reorder = TRUE, plot = TRUE, plot.type = "lollipop", error.bars = "sd", ylim = "auto", col = c("#4477aa", "#ee6677"), plot.points = TRUE, legend = TRUE, grid = TRUE, ...) {

  # version 1.5 (12 Jan 2023)

  # if 'col' has length 2 and varImp has negative values (e.g. for z-value), those will get the second colour

  stopifnot(imp.type == "each",
            plot.type %in% c("lollipop", "barplot", "boxplot"),
            # is.numeric(error.bars) || error.bars %in% c("sd", "range"),
            is.logical(TRUE),
            is.logical(plot),
            is.logical(plot.points),
            is.logical(legend),
            is.logical(grid),
            length(col) %in% 1:2
            )

  if (is(model, "glm")) {  #  && !is(model, "Gam")
    error.bars <- NA

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
    # requireNamespace("gbm")  # would require a suggest/depend
    if ("gbm" %in% .packages()) {
      error.bars <- NA
      ylab <- "Relative influence"
      smry <- summary(model, plotit = FALSE)
      varimp <- smry[ , "rel.inf"] / 100
      names(varimp) <- smry[ , "var"]
    } else {
      stop("package 'gbm' needs to be loaded first.")
    }
  }

  else if (is(model, "randomForest")) {
    error.bars <- NA
    varimp <- model$importance  # / nrow(model$importance) / 100  # doesn't work well for mean accuracy decrease
    names(varimp) <- rownames(model$importance)
    ylab <- colnames(model$importance)
  }

  else if (is(model, "bart")) {
    ylab <- "Proportion of splits used"
    varimps <- model$varcount / rowSums(model$varcount)
    varimp <- colMeans(varimps)
  }

  else stop ("'model' is of a non-implemented class.")

  if (reorder) {
    varimp <- varimp[order(abs(varimp), decreasing = TRUE)]
    if (is(model, "bart"))  varimps <- varimps[ , names(varimp)]
  }

  if (!is.na(error.bars)) {
    if (error.bars == "sd") {
      vsd <- sapply(as.data.frame(varimps), sd)
      eb_lower <- varimp - vsd
      eb_upper <- varimp + vsd
    } else if (error.bars == "range") {
      eb_lower <- apply(varimps, 2, min)
      eb_upper <- apply(varimps, 2, max)
    } else if (is.numeric(error.bars) && length(error.bars) == 1 && error.bars >= 0 && error.bars <= 1) {
      quants <- sapply(data.frame(varimps), quantile, probs = c(1 - error.bars, error.bars))
      eb_lower <- quants[1, ]
      eb_upper <- quants[2, ]
    } else stop ("Invalid 'error.bars' argument; see help for valid options.")
  }  # end if error.bars

  if (plot) {
    if (length(col) == 1) col <- rep(col, 2)
    colrs <- ifelse(varimp >= 0, col[1], col[2])

    if ("auto" %in% ylim) {
      ymin <- ifelse(!is.na(error.bars), min(varimps), min(abs(varimp)))
      ymax <- ifelse(!is.na(error.bars), max(varimps), max(abs(varimp)))
      ylim <- c(ymin, ymax)
    }

    if (plot.type == "lollipop") {
      sticks <- ifelse(is.na(error.bars), TRUE, FALSE)

      lollipop(abs(varimp),
               col = colrs,
               names = names(varimp),
               ylab = ylab,
               ylim = ylim,
               las = 2,
               sticks = sticks,
               grid = grid,
               ...)

      if (!is.na(error.bars)) {
        arrows(x0 = 1:length(varimp), x1 = 1:length(varimp), y0 = eb_lower, y1 = eb_upper, code = 3, angle = 90, length = 0.03, col = colrs)
      }
    }  # end if lollipop

    if (plot.type == "barplot") {
      space <- 0.25

      barplot(abs(varimp),
              col = colrs,
              border = NA,
              space = space,
              names = names(varimp),
              ylab = ylab,
              ylim = ylim,
              xpd = FALSE,
              las = 2,
              ...)

      if (grid) grid()

      if (plot.points || !is.na(error.bars)) {
        nbars <- length(names(varimp))
        xbars <- ((1 : nbars) - space) + space * (0 : (nbars - 1))
      }

      if (!is.na(error.bars)) {
        arrows(x0 = xbars, x1 = xbars, y0 = eb_lower, y1 = eb_upper, angle = 90, code = 3, length = 0.03, col = "#10133a")
      }
    }  # end if barplot

    if (plot.type == "boxplot") {
      if(is.na(error.bars)) vi <- t(as.data.frame(abs(varimp))) else vi <- varimps  # ifelse makes single boxplot for all vars

      boxplot(vi,
              col = adjustcolor(colrs, alpha.f = 0.2),
              border = colrs,
              ylab = ylab,
              ylim = ylim,
              las = 2,
              ...)

      if (grid) grid()
    }  # end if boxplot

    if (plot.points && is(model, "bart")) {
      if (plot.type == "barplot") xx <- rep(xbars, each = nrow(varimps))
      else xx <- rep(1:ncol(varimps), each = nrow(varimps))
      jj <- sapply(xx, jitter, amount = 0.1)
      points(x = jj, y = varimps, pch = 20, cex = 0.1, col = adjustcolor("#ffaabb", alpha.f = 0.3))
      if (plot.type == "lollipop") arrows(x0 = 1:length(varimp), x1 = 1:length(varimp), y0 = eb_lower, y1 = eb_upper, code = 3, angle = 90, length = 0.03, col = colrs)  # re-plot error bars on top for better visibility
    }

    signs <- unique(sign(varimp)[sign(varimp) != 0])  # check for both negative and positive varimps
    if (legend && length(signs) > 1) legend("topright", legend = c("positive", "negative"), fill = col, border = NA, bty = "n")
  }  # end if plot

  if (is.na(error.bars)) return(varimp)

  return (data.frame(Mean = varimp, Lower = eb_lower, Upper = eb_upper))
}
