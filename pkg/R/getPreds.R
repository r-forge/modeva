getPreds <-
function(data, models, y = FALSE, P = TRUE, Favourability = FALSE) {
  # version 1.4 (25 Mar 2014)
  # data: the data frame to which to add model predictions; must contain all variables (with the same names, case-sensitive) included in the models
  # models: a list of model objects obtained e.g. with function 'multGLM'
  # y: logical, whether to include the logit link (y) in the predictions
  # Favourability: logical, whether to include Favourability in the predictions; requires functions Fav and prevalence
  
  start.time <- Sys.time()

  stopifnot(
    is.data.frame(data),
    is.list(models),
    is.logical(y),
    is.logical(P),
    is.logical(Favourability)
  )
  
  if (!y & !P & !Favourability) stop("There are no predictions to get 
if all y, P and Favourability are set to FALSE.")
  
  keeP <- P  # keep P only if the user wants it
  if (Favourability)  P <- TRUE  # P is necessary to calculate Fav
  
  n.nulls <- length(models[sapply(models, is.null)])
  if (n.nulls > 0)  warning (n.nulls, " model(s) were NULL and therefore 
          did not generate predictions")
  models <- models[!sapply(models, is.null)]
  n.models <- length(models)
  mod.count <- 0
  
  for (m in 1:n.models) {
    mod.count <- mod.count + 1
    mod.name <- names(models)[m]
    message("Predicting with model ", mod.count, " of " , n.models, 
            " (", mod.name, ")...")
    if (y) {
      data[ , ncol(data) + 1] <- predict(models[[mod.count]], data)
      names(data)[ncol(data)] <- paste(mod.name, "y", sep = "_")
    }
    if (P) {
      data[ , ncol(data) + 1] <- predict(models[[mod.count]], data, 
                                         type = "response")
      names(data)[ncol(data)] <- paste(mod.name, "P", sep = "_")
    }
    if (Favourability) {
      data[ , ncol(data) + 1] <- Fav(pred = data[ , ncol(data)], 
                                   sample.preval = prevalence(models[[mod.count]]$y))
      names(data)[ncol(data)] <- paste(mod.name, "F", sep = "_")
      if (!keeP) data <- data[ , -(ncol(data) - 1)]
    }  # end if Fav
  }  # end for m

  duration <- difftime(start.time, Sys.time())
  units <- attr(duration, "units")
  duration <- round(abs(as.numeric(duration)), 1)
  message("Finished in ", duration, " ", units)
  return(data)
}
