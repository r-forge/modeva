confusionLabel <- function(model = NULL, obs = NULL, pred = NULL, thresh, verbosity = 2, na.rm = FALSE, rm.dup = FALSE) {
  # version 1.5 (6 May 2022)
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  res <- rep("", length(obs))
  res[pred >= thresh & obs == 1] <- "TruePos"
  res[pred < thresh & obs == 0] <- "TrueNeg"
  res[pred >= thresh & obs == 0] <- "FalsePos"
  res[pred < thresh & obs == 1] <- "FalseNeg"
  res
}
