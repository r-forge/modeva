\name{predPlot}
\alias{predPlot}
\title{
Plot predicted values for presences and absences, classified according to a prediction threshold.
}
\description{
This function plots predicted values separated into observed presences and absences and coloured according to whether they are above or below a given prediction threshold. The plot imitates (with permission from the author) one of the graphical outputs of the 'summary' of models built with the \pkg{embarcadero} package (Carlson, 2020), but it can be applied to any 'glm' object or any set of observed and predicted values, and it allows specifying a user-defined threshold.
}
\usage{
predPlot(model = NULL, obs = NULL, pred = NULL, thresh = "preval", 
main = "Classified predicted values", legend.pos = "n", pch = 1, col = c("black", "grey"))
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a vector of observed presences (1) and absences (0) of a binary response variable. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. This argument is ignored if 'model' is provided.}
  \item{thresh}{threshold value to separate predicted presences from predicted absences in 'pred'; can be "preval" (the default), to use the \code{\link{prevalence}} (i.e. proportion of presences) in 'obs', or any real number between 0 and 1. This value will be used to draw a vertical line on the plot and to colour the points (predicted values) according to whether they fall below or above the threshold. See Details in help(threshMeasures) for an informed choice.}
  \item{main}{Main title for the plot.}
  \item{legend.pos}{character value specifying the position for the legend on the plot. Can be "bottomleft", "bottom", "bottomright", "topleft", "left", "top", "topright", "right", "center", or NA or "n" for no legend (the default). Partial argument matching is used.}
  \item{pch}{plotting character for the presences and absences (see \code{\link{points}}).}
  \item{col}{vector of length 2 indicating the colours with which to plot predicted presences and absences (points above and below the threshold), respectively.}
}
\note{
Points are \code{\link{jitter}}ed randomly along the y axis to minimize visual overlap. So, each run of 'predPlot' (unless you use \code{\link{set.seed}} first) will produce a different arrangement of points for the same data, although their x-axis values are faithful.
}
\value{
This function outputs a plot as per 'Description'.
}
\references{
Carlson C.J. (2020) embarcadero: Species distribution modelling with Bayesian additive regression trees in R. Methods in Ecology and Evolution, 11: 850-858.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{predDensity}}, \code{\link{plotGLM}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

predPlot(model = mod)

predPlot(model = mod, thresh = 0.5)


# you can first select a threshold optimized according to a particular metric:

\dontrun{
threshold <- optiThresh(mod, measures = "TSS", optimize = "each")
threshold <- threshold$optimals.each[ , "threshold"]
threshold
predPlot(model = mod, thresh = threshold)
}

# you can also use 'predPlot' with vectors of observed and predicted values
# instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

predPlot(obs = presabs, pred = prediction)

predPlot(obs = presabs, pred = prediction, thresh = 0.5)
}
