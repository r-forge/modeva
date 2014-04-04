FDR <-
function(response, predictors, model.type, pvalues = NULL, 
                family = "binomial", correction = "fdr", q = 0.05) {
  
  # version 1.9 (3 Sep 2013)
  # response: a vector or column with your response variable
  # predictors: the vectors or columns of your predictor variables
  # model.type: either "LM" (linear model, for continuous variables) or "GLM" (generalized linear models, for binary or other variables for which these models are more appropriate)
  # pvalues: optionally, INSTEAD of response and preditors, a table with the names of the predictors in the first column and their univariate p-values (obtained elsewhere) in the second column. Dummy example: pvalues <- data.frame(var = letters[1:5], pval = runif(5, 0, 0.4))
  # family: in the case of GLM, the error distribution and link function (type ?glm or ?family for details); the default is "binomial" (for binary logistic regression)
  # correction: the correction procedure to apply to the p-values; type p.adjust.methods for available options, and ?p.adjust for more information
  # q: the threshold value of FDR-corrected significance above which to reject variables
  
  if (missing(model.type)) stop ("'model.type' is missing")
  if (!(correction %in% p.adjust.methods)) stop("Invalid correction method. 
Type 'p.adjust.methods' for available options.")
  
  if (!is.null(pvalues)) {
    coeffs <- FALSE
    p.bivar <- pvalues[ , 2]
    names(p.bivar) <- pvalues[ , 1]
  }  # end if pvalues
  
  else {
    coeffs <- TRUE
    if (NA %in% response | NA %in% predictors) 
      stop("Please remove (rows with) missing values from your data; 
           note that 'response' and 'predictors' must have the same number 
           of observations and in the same order.")
    if (is.null(ncol(predictors))) 
      stop("You need more than one predictor to calculate the FDR.")
    if (!is.vector(response) | length(response) != nrow(predictors)) 
      stop("'response' must be a vector and have the same length as the number 
           of rows in 'predictors'.")
    
    p.bivar <- coef.bivar <- vector("numeric", length = ncol(predictors))
    
    for (i in 1:length(p.bivar)) {
      if(model.type == "GLM") {
        model <- glm(response ~ predictors[ , i], family = family)
        p.bivar[i] <- anova(model, test = "Chi") $ P[2]
        coef.bivar[i] <- model $ coefficients[2]
      }  # end if GLM
      
      else if (model.type == "LM") {
        model <- lm(response ~ predictors[ , i])
        p.bivar[i] <- anova(model, test = "Chi") $ P[1]
        coef.bivar[i] <- model $ coefficients[2]
      }  # end if LM
      
      else stop("'model.type' must be either 'LM' or 'GLM'")
    } # end for i in p.bivar
    
    if (is.na(p.bivar[i])) message("A p-value could not be calculated for the ", 
                                   i, "th column in 'predictors'.", sep = "")
    
  }  # end else
  
  if (coeffs) {
    results <- data.frame(cbind(coef.bivar, p.bivar), 
                          row.names = names(predictors))
    names(results) <- c("bivariate.coeff", "p.value")
    results <- results[order(results$p.value), ]
    results$p.adjusted <- p.adjust(results$p.value, method = correction)
  }  # end if coeffs
  else {
    results <- data.frame(p.value = p.bivar, row.names = pvalues[ , 1])
    results <- data.frame(p.value = results[order(results$p.value), ])
    results$p.adjusted <- p.adjust(results$p.value, method = correction)
  }  # end if coeffs else
  
  select <- subset(results, p.adjusted <= q)
  exclude <- subset(results, p.adjusted > q)
  cat("\nBivariate p-values adjusted with '", correction, 
      "' correction;\n", nrow(select), " variables selected, ", 
      nrow(exclude), " excluded\n\n", sep = "")
  
  return(list(select = select, exclude = exclude))
}
