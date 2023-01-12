\name{varImp}
\alias{varImp}
\title{Variable importance.}
\description{
This function gets, and optionally plots, variable importance for an input model object of an implemented class.
}
\usage{
  varImp(model, imp.type = "each", reorder = TRUE, plot = TRUE,
  plot.type = "lollipop", error.bars = "sd", ylim = "auto", col = c("#4477aa",
  "#ee6677"), plot.points = TRUE, legend = TRUE, grid = TRUE, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gbm", "randomForest" or "bart".}
  \item{imp.type}{character value indicating the type of variable importance to output, i.e. the metric with which importance is measured. Currently the only option is "each", to extract the measure provided within each model object. Note that this is inconsistent across model classes -- see Details.}
  \item{reorder}{logical value indicating whether to sort the variables in decreasing order of importance. TRUE by default. If set to FALSE, the variables will retain their input order.}
  \item{plot}{logical value indicating whether to produce a plot with the results.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "\code{\link{lollipop}}" (the default), "\code{\link{barplot}}", or "\code{\link{boxplot}}". Note that the latter is only useful when 'model' contains several importance values per variable (i.e. for models of class "bart").}
  \item{error.bars}{character value indicating the type of error metric to compute (and plot, if plot=TRUE) if the input contains the necessary information (i.e., for models of class "bart") and if the 'plot.type' is appropriate (i.e. "lollipop" or "barplot"). Can be "sd" (the default) for the standard deviation; "range" for the minimum and maximum value across the ones available; a numeric value between 0 and 1 for the corresponding confidence interval (e.g. 0.95 for 95\%), computed with \code{\link{quantile}}; or NA for no error.bars.}
  \item{ylim}{either a numeric vector of length 2 specifying the limits (minimum, maximum) for the y axis, or "auto" (the default) to fit the axis to the existing values.}
  \item{col}{character or integer vector of length 1 or 2 specifying the plotting colours (if plot=TRUE) for the variables with positive and negative effect on the response, when this info is available (e.g. for models of class "glm").}
  \item{plot.points}{logical, whether or not to add to the plot the individual importance points (rather than just the mean importance value, and the error bar if error.bars=TRUE) for each variable. By default it is TRUE (following Weissgerber et al. 2015), but it only holds for model objects that include several possible importance values per variable (e.g. class "bart").}
  \item{legend}{logical, whether or not to draw a legend. Used only if plot=TRUE and if the output includes negative values (i.e., if 'model' is of class 'GLM' and has variables with positive and negative coefficients).}
  \item{grid}{logical, whether or not to add a grid to the plot. The default is TRUE.}
  \item{\dots}{additional arguments that can be used for the plot, e.g. 'main', 'cex.axis', 'cex.names'.}
}
\details{
  Variable importance in a model of class "glm" obtained with the \code{\link{glm}} function can be measured by the magnitude of the absolute z-value test statistic, which is provided with \code{summary(model)}. The 'varImp' function outputs the absolute z value of each variable, and in the plot (by default) uses different colours for variables with positive and negative relationships with the response.
  If the input model is of class "gbm" of the \pkg{gbm} package, variable importance is obtained from \code{summary.gbm(model)} and divided by 100 to get the result as a proportion rather than a percentage.
  If the input model is of class "randomForest" of the \pkg{randomForest} package, variable importance is obtained with \code{model$importance}.
  If the input model is of class "bart" of the \pkg{dbarts} package, variable importance is obtained as the mean number of tree splits each variable was chosen for. If 'error.bars' is not NA, the error is also computed according to the specified metric ("sd" or standard deviation by default).
  }
\value{
  This function outputs, and optionally plots, a named numeric vector of variable importance, as measured by 'imp.type' (see Details). If 'model' is of class "bart" and 'error.bars' is not NA, the output is a row-named data frame with the mean and the lower and upper bounds of the error bars of variable importance.
}
\author{
  A. Marcia Barbosa
}
\references{
Weissgerber T.L., Milic N.M., Winham S.J. & Garovic V,D, (2015) Beyond Bar and Line Graphs: Time for a New Data Presentation Paradigm. PLOS Biol 13:e1002128. https://doi.org/10.1371/JOURNAL.PBIO.1002128
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
  varImp(model = mod,
    col = c("darkgreen", "gold"),
    plot.type = "barplot",
    main = "Variable importance in \n my model")
}
