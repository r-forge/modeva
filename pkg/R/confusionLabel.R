confusionLabel <- function(model = NULL, obs = NULL, pred = NULL, thresh, verbosity = 2, na.rm = FALSE, rm.dup = FALSE) {
  # version 1.6 (16 May 2022)
  
  pred_in <- pred  # in case input is raster, so final reclass is also raster
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  res <- rep("", length(obs))
  res[pred >= thresh & obs == 1] <- "TruePos"
  res[pred < thresh & obs == 0] <- "TrueNeg"
  res[pred >= thresh & obs == 0] <- "FalsePos"
  res[pred < thresh & obs == 1] <- "FalseNeg"
  
  if (inherits(pred_in, "SpatRaster")) {
    finite_pixels <- is.finite(terra::values(pred_in))
    terra::values(pred_in)[finite_pixels] <- as.integer(as.factor(res))
    res <- pred_in
  }
  
  res
}
