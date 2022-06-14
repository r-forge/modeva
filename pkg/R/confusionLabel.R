confusionLabel <- function(model = NULL, obs = NULL, pred = NULL, thresh, interval = interval, quant = quant, verbosity = 2, na.rm = FALSE, rm.dup = FALSE) {
  # version 1.7 (14 Jun 2022)
  
  pred_in <- pred  # in case input is raster, so final reclass is also raster
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))
    stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm)
  
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
