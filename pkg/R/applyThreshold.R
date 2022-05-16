applyThreshold <- function(model = NULL, obs = NULL, pred = NULL, thresh, right = FALSE, interval = 0.01, quant = 0, na.rm = TRUE) {
  
  # version 1.0 (16 May 2022)
  
  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))
    stop("'thresh' must be either a numeric value or one of the options obtained with modEvAmethods('getThreshold')")
  
  pred_in <- pred  # in case input is raster, so final reclass is also raster
  
  obspred <- inputMunch(model, obs, pred)
  if (!is.null(obs) || !is.null(model)) obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm)
  
  if (inherits(pred_in, "SpatRaster")) reclass <- pred_in
  else reclass <- pred
  reclass[reclass > thresh] <- 1
  reclass[reclass < thresh] <- 0
  if (right) reclass[reclass == thresh] <- 0
  else reclass[reclass == thresh] <- 1
  
  return(reclass)
}
