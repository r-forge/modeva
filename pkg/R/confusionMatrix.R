confusionMatrix <- function(model = NULL, obs = NULL, pred = NULL, thresh, interval = interval, quant = quant, verbosity = 2, na.rm = TRUE, rm.dup = FALSE) {
  # version 1.0 (14 Jun 2022)
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))
    stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm)
  
  obs0 <- obs == 0
  obs1 <- obs == 1
  pred0 <- pred < thresh
  pred1 <- pred >= thresh
  a <- sum(obs1 & pred1, na.rm = na.rm)
  b <- sum(obs0 & pred1, na.rm = na.rm)
  c <- sum(obs1 & pred0, na.rm = na.rm)
  d <- sum(obs0 & pred0, na.rm = na.rm)
  
  out <- data.frame(obs1 = c(a, c), obs0 = c(b, d))
  rownames(out) <- c("pred1", "pred0")
  out
}
