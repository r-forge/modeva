confusionLabel <- function(model = NULL, obs = NULL, pred = NULL, thresh, interval = 0.01, quant = 0, verbosity = 2, na.rm = FALSE, rm.dup = FALSE) {
  # version 1.8 (16 Jan 2024)

  pred_in <- pred  # in case input is raster, so final reclass is also raster

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))  stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm)

  output <- rep("", length(obs))
  output[pred >= thresh & obs == 1] <- "TruePos"
  output[pred < thresh & obs == 0] <- "TrueNeg"
  output[pred >= thresh & obs == 0] <- "FalsePos"
  output[pred < thresh & obs == 1] <- "FalseNeg"

  if (inherits(pred_in, "SpatRaster")) {
    finite_pixels <- which(is.finite(terra::values(pred_in)))
    output_rast <- pred_in
    levs <- as.factor(output)
    terra::values(output_rast)[finite_pixels] <- levs  # only 'finite_pixels' because output is shorter when input has NAs
    levels(output_rast) <- data.frame(id = unique(as.integer(levs)), cat = unique(output))
    output <- output_rast
  }

  output
}
