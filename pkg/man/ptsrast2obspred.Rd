\name{ptsrast2obspred}
\alias{ptsrast2obspred}
\title{
Extract observed and predicted values from presence points and a prediction raster map.
}
\description{
This function takes presence coordinates and a raster map of model predictions returns two paired numeric vectors of the observed and predicted values for each pixel.
}
\usage{
ptsrast2obspred(pts, rst, rm.dup = FALSE)
}
\arguments{
  \item{pts}{a 'SpatVector' map of the presence points, or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively.}
  \item{rst}{a one-layer 'SpatRaster' map of the model predictions, in the same CRS as 'pts'.}
  \item{rm.dup}{logical, whether additional points within the same pixel should be removed. See Examples.}
}
\value{
A data frame with one column containing the observed (1 for presence, 0 for absence) and another column containing the predicted values.
}
\author{
A. Marcia Barbosa
}
\examples{
# you can run these examples if you have the 'terra' package installed
require(terra)

# import a system raster with 3 layers and crop it to a smaller extent:
rst <- terra::rast(system.file("ex/logo.tif", package="terra"))
ext <- terra::ext(c(0, 5, 26, 28))
rst <- terra::crop(rst, ext)
plot(rst[[1]])

# generate some random presence points:
set.seed(2)
presences <- terra::spatSample(as.polygons(ext(c(0, 5, 25, 30))), 10, method = "random")
plot(rst[[1]])
points(presences, pch = 20, cex = 0.3, col = "black")


# use 'ptsrast2obspred' on these points and raster:
# (with and without removing duplicates)

obspred <- ptsrast2obspred(pts = crds(presences), rst = rst[[1]])
obspred
nrow(obspred)  # you get as many 'obs' as pixels + additional points per pixel
sum(obspred$obs)  # as many presences as points that overlay 'pred'

obspred_rmdup <- ptsrast2obspred(pts = crds(presences), rst = rst[[1]], rm.dup = TRUE)
obspred_rmdup
nrow(obspred_rmdup)  # you get as many 'obs' as pixels
sum(obspred_rmdup$obs)  # as many presences as pixels that contain (one or more) points
}
