Boyce <- function(model = NULL, obs = NULL, pred = NULL, nclass = 0, window.w = "default", res = 100, method = "spearman", rm.dupl.classes = TRUE, rm.dupl.points = TRUE, plot = TRUE, plot.lines = TRUE, plot.values = TRUE, plot.digits = 3) {
  
  if (!is.null(model)) {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obspred <- mod2obspred(model)
    obs <- obspred[ , "obs"]
    pred <- obspred[ , "pred"]
  }  # end if model
  
  if (inherits(pred, "SpatRaster")) {
    
    error_message <- "If 'pred' is a SpatRaster, 'obs' must be either a 'SpatVector' of the presence points, or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively."
    if (!(inherits(obs, "data.frame") || inherits(obs, "matrix"))) stop(error_message)
    if ((inherits(obs, "data.frame") || inherits(obs, "matrix")) && ncol(obs) != 2) stop(error_message)

    obspred <- ptsrast2obspred(pts = obs, rst = pred, rm.dup = rm.dupl.points)
    obs <- obspred[ , "obs"]
    pred <- obspred[ , "pred"]
  }  # end if SpatRaster
  
  dat <- data.frame(obs, pred)
  n.in <- nrow(dat)
  dat <- na.omit(dat)
  n.out <- nrow(dat)
  if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
  obs <- dat$obs
  pred <- dat$pred
  
  # to match the original 'ecospat::ecospat.boyce' arguments:
  fit <- pred
  obs <- pred[which(obs == 1)]
  
  # if (class(fit) == "RasterLayer") {
  #   if (class(obs) == "data.frame" || class(obs) == "matrix") {
  #     obs <- extract(fit, obs)
  #   }
  #   fit <- getValues(fit)
  #   fit <- fit[!is.na(fit)]
  # }
  
  if (inherits(fit, "SpatRaster")) {
    if (inherits(obs, "data.frame") || inherits(obs, "matrix") || inherits(obs, "SpatVector")) {
      obs <- terra::extract(fit, obs)
    } else {
      stop("When 'pred' is a 'SpatRaster', 'obs' must be either a 'SpatVector' of the presence points or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively.")
    }
    fit <- na.omit(terra::values(fit))
  }
  
  # the remainder of the function is slightly modified from 'ecospat::ecospat.boyce':
  
  boycei <- function(interval, obs, fit) {
    pi <- sum(as.numeric(obs >= interval[1] & obs <= interval[2])) / length(obs)
    ni <- sum(as.numeric(fit >= interval[1] & fit <= interval[2]))  # my add
    ei <- ni / length(fit)
    #return(round(pi / ei, 10))  # my removal
    #return(rbind(boycei = round(pi / ei, 10), bin.N = ni))  # my add
    return(rbind(bin.N = ni, predicted = pi, expected = ei, boycei = round(pi / ei, 10)))  # my add
  }
  
  mini <- min(fit, obs)
  maxi <- max(fit, obs)
  if (length(nclass) == 1) {
    if (nclass == 0) {
      if (window.w == "default") {
        window.w <- (max(fit) - min(fit)) / 10
      }
      vec.mov <- seq(from = mini, to = maxi - window.w, by = (maxi - mini - window.w) / res)
      vec.mov[res + 1] <- vec.mov[res + 1] + 1
      interval <- cbind(vec.mov, vec.mov + window.w)
    } else {
      vec.mov <- seq(from = mini, to = maxi, by = (maxi - mini) / nclass)
      interval <- cbind(vec.mov, c(vec.mov[-1], maxi))
    }
  } else {
    vec.mov <- c(mini, sort(nclass[!nclass > maxi | nclass < mini]))
    interval <- cbind(vec.mov, c(vec.mov[-1], maxi))
  }
  
  boycei.result <- t(apply(interval, 1, boycei, obs, fit))  # my add
  
  f <- boycei.result[ , 4]  # added '[,4]' as per my 'boycei' return modification
  
  to.keep <- which(!is.nan(f))  # changed from f!="NaN"
  f <- f[to.keep]
  
  if (length(f) < 2) {
    b <- NA
  } else {
    r <- 1:length(f)
    if (rm.dupl.classes) {
      r <- c(1:length(f))[f != c(f[-1], TRUE)]
    }
    b <- cor(f[r], vec.mov[to.keep][r], method = method)
  }
  
  HS <- apply(interval, 1, sum) / 2
  if (length(nclass) == 1 & nclass == 0) {
    HS[length(HS)] <- HS[length(HS)] - 1
  }
  HS <- HS[to.keep]
  
  if (plot) {
    plot(HS, f, xlab = "Prediction class", ylab = "Predicted / expected ratio", col = "grey", cex = 0.5)  # includes duplicate P/E values
    if (plot.lines) {  # my add
      lines(HS, f, col = "grey")
      lines(HS[r], f[r])
    }  # my add
    points(HS[r], f[r], pch = 19, cex = 0.5)  # without duplicate P/E values
    #abline(h = 1, lty = 5, col = "grey")  # my add
    if (plot.values) text(x = median(range(HS)), y = diff(range(f)) / 20, paste("B =", round(b, plot.digits)), adj = c(0.5, 1))  # my add
  }
  
  # the following is different from 'ecospat.boyce':
  return(list(bins = data.frame(bin.N = boycei.result[to.keep, 1],
                                bin.min = interval[to.keep, 1], 
                                bin.max = interval[to.keep, 2], 
                                bin.median = HS, 
                                predicted = boycei.result[to.keep, 2],
                                expected = boycei.result[to.keep, 3],
                                PE.ratio = f), 
              Boyce = b))
}
