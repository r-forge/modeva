\name{varImp}
\alias{varImp}
\title{Variable importance.}
\description{
This function gets, and optionally plots, variable importance for an input model object.
}
\usage{
  varImp(model, reorder = TRUE, plot = TRUE, plot.type = "lollipop",
  ylim = "auto", col = c("darkturquoise", "salmon"), ci = TRUE, ci.type = "sd",
  legend = TRUE, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gbm", "randomForest" or "bart".}
  \item{reorder}{logical value indicating whether to reorder variables in decreasing order of importance. TRUE by default. If set to FALSE, the variables will retain their input order.}
  \item{plot}{logical value indicating whether to also produce a plot.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "barplot" or "lollipop" (the default).}
  \item{ylim}{either a numeric vector of length 2 specifying the limits (minimum, maximum) for the y axis, or "auto" (the default) to fit the axis to the existing values.}
  \item{col}{character or integer vector of length 1 or 2 specifying the plotting colours (if plot=TRUE) for the variables with positive and negative effect on the response, when available (e.g. for GLM).}
  \item{ci}{logical, whether or not to plot a confidence interval for variable importance. By default it is TRUE, but it only applies to input models of class "bart".}
  \item{ci.type}{character vector indicating the type of confidence interval to compute. Can be "range" for the minimum and maximum value across the prediction trees, or "sd" for the standard deviation.}
  \item{legend}{logical, whether or not to add a legend. Used only if plot=TRUE and if the output includes negative values.}
  \item{\dots}{additional arguments that can be used for the plot, e.g. 'main', 'cex.axis', 'cex.names'.}
}
\details{
  Variable importance in a model of class "glm" obtained with the \code{\link{glm}} function can be measured by the magnitude of the absolute z-value test statistic, which is provided with \code{summary(model)}. The 'varImp' function outputs the absolute z value of each variable, and in the plot (by default) uses different colours for variables with positive and negative relationships with the response.
  If the input model is of class "gbm" of the \pkg{gbm} package, variable importance is obtained from \code{summary.gbm(model)} and divided by 100 to get the result as a proportion rather than a percentage.
  If the input model is of class "randomForest" of the \pkg{randomForest} package, variable importance is obtained with \code{model$importance}.
  If the input model is of class "bart" of the \pkg{dbarts} package, variable importance is obtained as the mean number of tree splits each variable was chosen for. If ci=TRUE, confidence intervals are also obtained.
  }
\value{
  This function outputs, and optionally plots, a named numeric vector of variable importance, as measured by the absolute z-value.
}
\author{
  A. Marcia Barbosa
}
\seealso{
  \code{\link{summary.glm}}
}
\examples{
  # load sample models:
  data(rotif.mods)

  # choose a particular model to play with:
  mod <- rotif.mods$models[[1]]


  # get variable importance for this model:
  varImp(model = mod)

  # change some parameters:
  par(mar = c(10, 4, 2, 1))
  varImp(model = mod, col = c(1, 2), main = "Variable importance in \n my model")
}
