ptsrast2obspred <- function(pts, rst, rm.dup = FALSE) {
  if (!(inherits(pts, "SpatVector") || inherits(pts, "data.frame") || inherits(pts, "matrix")) || (inherits(pts, "data.frame") || inherits(pts, "matrix") && ncol(pts) != 2)) stop("'pts' must be either a 'SpatVector' of the presence points, or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively.")
  if (terra::nlyr(rst) > 1) stop("'rst' must have only one layer.")
  
  if (inherits(pts, "data.frame")) pts <- as.matrix(pts)  # as per 'terra::rasterize' input requirements

  obs_rst <- terra::rasterize(pts, rst, fun = sum)
  obs_rst[is.na(obs_rst)] <- 0L

  obs <- terra::values(obs_rst)[ , 1]
  pred <- terra::values(rst)[ , 1]

  out <- data.frame(obs = obs, pred = pred)
  
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
