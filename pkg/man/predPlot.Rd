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
main = "Classified predicted values", legend.pos = "bottomright")
}
\arguments{
  \item{model}{a model object of class "glm" and family "binomial".}
  \item{obs}{(instead of a 'model' object) numeric vector of the observed data, consisting of zeros (absences) and ones (presences). This argument is ignored if 'model' is provided.}
  \item{pred}{(instead of a 'model' object) numeric vector of the values predicted by a model of the 'obs'erved data. This argument is ignored if 'model' is provided. Must be of the same length and in the same order as 'obs'.}
  \item{thresh}{threshold value to separate predicted presences from predicted absences in 'pred'; can be "preval" (the default), to use the \code{\link{prevalence}} (i.e. proportion of presences) in 'obs', or any real number between 0 and 1. This value will be used to draw a vertical line on the plot and to colour the points (predicted values) according to whether they fall below or above the threshold. See Details in help(threshMeasures) for an informed choice.}
  \item{main}{Main title for the plot.}
  \item{legend.pos}{character value specifying the position for the legend on the plot. Can be "bottomright" (the default), "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center", or NA or "n" for no legend. Partial argument matching is used.
  }
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
