\name{Boyce}
\alias{Boyce}
\title{
Boyce Index
}
\description{
This function computes the (continuous) Boyce index (Boyce 2002; Hirzel et al. 2006) for either: 1) a model object, or 2) two paired numeric vectors of observed (binary, 1 for occurrence vs. 0 for no occurrence records) and predicted (continuous, e.g. occurrence probability) values, or 3) a set of presence point coordinates and a raster map with the predicted values for the entire model evaluation area. This metric is designed for evaluating model predictions against presence/background data (i.e. presence/available, where "available" includes both presences and absences; Boyce 2002), so the function uses the model predictions for the presence sites (ones) against the predictions for the entire dataset (ones and zeros). The code is largely based on the 'ecospat.boyce' function in the \pkg{ecospat} package (version 3.2.1), but it is modified to match the input types in the remaining functions of 'modEvA', and to return a more complete output.
}
\usage{Boyce(model = NULL, obs = NULL, pred = NULL, nclass = 0, 
window.w = "default", res = 100, method = "spearman", rm.dupl.classes = TRUE, rm.dupl.points = TRUE,
plot = TRUE, plot.lines = TRUE, plot.values = TRUE, plot.digits = 3)}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments (e.g. for external test data) instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), you can provide a SpatVector map of the presence points or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a 'SpatVector' or a set of point coordinates), you can provide a 'SpatRaster' map of the predicted values (obtained withe the \code{rast()} function of the \pkg{terra} package) for the entire evaluation region. This argument is ignored if 'model' is provided.}
  \item{nclass}{The number of classes or bins (e.g. 10) in which to group the 'pred' values, or a vector with the bin thresholds. If \code{nclass = 0} (the default), a moving window is used (see next parameters), so as to compute the "continuous Boyce index" (Hirzel et al. 2006).}
  \item{window.w}{The width of the moving window. By default, it is 1/10 of the 'pred' range).}
  \item{res}{The resolution of the moving window (if nclass = 0). By default it is 100 focals, providing 100 moving bins).}
  \item{method}{argument to be passed to \code{\link{cor}} indicating which correlation coefficient to use. The default is \code{'spearman'} as per Boyce et al. (2002), but \code{'pearson'} and \code{'kendall'} can also be used.}
  \item{rm.dupl.classes}{If \code{TRUE} (the default, as per Hirzel et al. 2006) and if there are different bins with the same predicted/expected ratio, only one of each is used to compute the correlation. See Details.}
  \item{rm.dupl.points}{If \code{TRUE} (the default) and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}.}
  \item{plot}{logical, whether or not to plot the predicted/expected ratio against the median prediction of each bin. Defaults to TRUE.}
  \item{plot.lines}{logical, whether or not to add lines connecting the points in the plot (if plot=TRUE). Defaults to TRUE.}
  \item{plot.values}{logical, whether or not to show in the plot the value of the Boyce index. Defaults to TRUE.}
  \item{plot.digits}{number of digits to which the value in the plot should be \code{\link{round}}ed (if 'plot' and 'plot.values' are TRUE). Defaults to 3.}
}
\details{
The Boyce index is the correlation between model predictions and area-adjusted frequencies (i.e., observed vs. expected proportion of occurrences) along different prediction classes. In other words, it measures how model predictions differ from a random distribution of the observed presences across the prediction gradient (Boyce et al. 2002). It can take values between -1 and 1. Positive values indicate that presences are more frequent than expected by chance (given availability) in areas with higher predicted values. Values close to zero mean that predictions are no better than random, and negative values indicate counter predictions, i.e. that presences are more frequent in areas with lower predicted values.

Excluding bins with duplicated P/E values (\code{rm.dupl.classes = TRUE}) focuses more on the discriminative aspect of the predictions, lowering the assessment of the evaluation of the model resolution (sensu Hirzel et al. 2006 p. 150).
}

\value{
This function returns a \code{list} with the following components:
\item{bins}{a data frame with the number of values in each bin, their median and range of predicted values, and the corresponding predicted/expected ratio of presences.}
\item{B}{the numeric value of the Boyce index, i.e. the coefficient of correlation between the median predicted value in each bin and the corresponding predicted/expected ratio.}
If plot=TRUE (the default), the function also plots the predicted/expected ratio for the utilized bins along the prediction range. A good model provides a monotonically increasing curve. 
}
\author{
A. Marcia Barbosa, with large chunks of code from the 'ecospat::ecospat.boyce' function by Blaise Petitpierre and Frank Breiner (\pkg{ecospat} package version 3.2.1).
}
\references{
Boyce, M.S., P.R. Vernier, S.E. Nielsen & F.K.A. Schmiegelow (2002) Evaluating resource selection functions. Ecological Modelling 157: 281-300

Hirzel, A.H., G. Le Lay, V. Helfer, C. Randin & A. Guisan (2006) Evaluating the ability of habitat suitability models to predict species presences. Ecological Modelling 199: 142-152
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

# compute the Boyce index:
Boyce(model = mod)


# you can also use vectors of observed and predicted values
# instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

Boyce(obs = presabs, pred = prediction)
}