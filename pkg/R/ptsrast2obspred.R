ptsrast2obspred <- function(pts, rst, rm.dup = FALSE, na.rm = TRUE) {
  
  error_message <- "'pts' must be either a 'SpatVector' of the presence points, or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively."
  
  if (!(inherits(pts, "SpatVector") || inherits(pts, "data.frame") || inherits(pts, "matrix"))) stop (error_message)
  if ((inherits(pts, "data.frame") || inherits(pts, "matrix")) && ncol(pts) != 2) stop (error_message)
  
  if (!inherits(rst, "SpatRaster")) stop("'rst' must be of class SpatRaster. You can try converting it with terra::rast()")
  if (terra::nlyr(rst) > 1) stop("currently, 'rst' must have only one layer.")
  
  if (inherits(pts, "data.frame")) pts <- as.matrix(pts)  # as per 'terra::rasterize' input requirements
  
  obs_rst <- terra::rasterize(pts, rst, fun = sum)
  obs_rst[is.na(obs_rst)] <- 0L
  
  obs <- terra::values(obs_rst, mat = FALSE)
  pred <- terra::values(rst, mat = FALSE)
  
  out <- data.frame(obs = obs, pred = pred)
  
  # always remove absences outside value pixels:
  out <- out[obs > 0 | is.finite(pred), ]
  
  if (na.rm) {  # remove also presences outside value pixels:
    out <- out[which(is.finite(out$pred)), ]
  }
  
  if (rm.dup) {
    out$obs[out$obs > 1] <- 1L
  } else {
    repeats <- out$obs
    repeats[repeats == 0] <- 1L
    out <- out[rep(seq_len(nrow(out)), repeats), ]
    out$obs[out$obs > 1] <- 1L
  }
  
  return(out)
}
