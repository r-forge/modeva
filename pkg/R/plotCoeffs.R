plotCoeffs <- function(model, ...) {
  
  smry <- summary(model)$coefficients
  intercept <- grep("intercept", tolower(rownames(smry)))
  if (length(intercept) > 0)
    smry <- smry[-intercept, , drop = FALSE]
  
  if (nrow(smry) == 0) {
    # args <- list(...)
    # # plot.new(...)
    # # args_not_base_plot <- grep("horiz", names(args))
    # # if ("xlab" %in% names(args)) xlab <- args$xlab else xlab <- ""
    # # if ("ylab" %in% names(args)) ylab <- args$ylab else ylab <- ""
    # # plot(0:1, 0:1, type = "n", axes = FALSE, xlab = xlab, ylab = ylab, ...)
    # plot(0:1, 0:1, type = "n", axes = FALSE, ...)
    # text(x = 0.5, y = 0.5, "No variables in model.")
    return(NULL)
  }
  
  # the following may not work correctly for all model summaries!
  # tested with classes glm, lm, phylolm (as per help file)
  coef_column <- grep("estimate", tolower(colnames(smry)))
  se_column <- grep("err", tolower(colnames(smry)))
  pval_column <- which(startsWith(tolower(colnames(smry)), "p"))

  coefs <- smry[ , coef_column, drop = FALSE]
  se <- smry[ , se_column, drop = FALSE]
  lowerci <- coefs - 1.96 * se
  upperci <- coefs + 1.96 * se
  pvals <- smry[ , pval_column, drop = FALSE]
  
  lollipop(coefs, names = rownames(coefs), sticks = cbind(lowerci, upperci), bold = ifelse(pvals <= 0.05, TRUE, FALSE), ...)
  box()
}
