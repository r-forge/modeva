\name{AUC}
\alias{AUC}
\title{
Area Under the ROC Curve
}
\description{
This function calculates the Area Under the Curve of the receiver operating characteristic (ROC) plot, for either a model object of class \code{"glm"}, or two matching vectors of observed (binary, 1 for occurrence vs. 0 for non-occurrence) and predicted (continuous, e.g. occurrence probability) values, respectively. The AUC is a measure of the overall discrimination power of the predictions, or the probability that an occurrence site has a higher predicted value than a non-ocurrence site.
}
\usage{
AUC(model = NULL, obs = NULL, pred = NULL, simplif = FALSE, 
interval = 0.01, FPR.limits = c(0, 1), plot = TRUE, plot.values = TRUE, 
plot.digits = 3, plot.preds = FALSE, grid = FALSE, 
xlab = c("False positive rate", "(1-specificity)"),
ylab = c("True positive rate", "(sensitivity)"), main = "ROC curve", ...)
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another
  binary response variable. This argument is ignored if \code{model} is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as \code{obs}. This argument is ignored if \code{model} is provided.}
  \item{simplif}{logical, whether to use a faster version that outputs only the AUC value (may be useful for calculating AUC for large numbers of species in a loop, for example). Note that the ROC plot is not drawn when simplif = TRUE.}
  \item{FPR.limits}{(NOT YET IMPLEMENTED) numerical vector of length 2 indicating the limits of false positive rate between which to calculate a partial AUC. The default is c(0, 1), for considering the whole AUC.}
  \item{interval}{interval of threshold values at which to calculate the true and false positive and negative rates. Defaults to 0.01. This argument is ignored if \code{simplif = TRUE}. Note that this does not affect the obtained AUC value (although it can affect the size of the plotted ROC curve, especially when prevalence is low), as the AUC is calculated with the Mann-Whitney U statistic and is therefore threshold-independent.}
  \item{plot}{logical, whether or not to plot the ROC curve. Defaults to TRUE.}
  \item{plot.values}{logical, whether or not to show in the plot the values associated to the curve (e.g., the AUC). Defaults to TRUE.}
  \item{plot.digits}{integer number indicating the number of digits to which the values in the plot should be \code{\link{round}}ed. Defaults to 3. This argument is ignored if \code{plot} or \code{plot.values} are set to \code{FALSE}.}
  \item{plot.preds}{logical, whether or not to plot the proportion of analysed model predictions (through proportionally sized circles) at each threshold. Experimental. Defaults to FALSE.}
  \item{grid}{logical, whether or not to add a grid to the plot, marking the analysed thresholds. Defaults to FALSE.}
  \item{xlab}{label for the x axis.}
  \item{ylab}{label for the y axis.}
  \item{main}{title for the plot.}
  \item{\dots}{further arguments to be passed to the \code{\link{plot}} function.}
}
\details{
Like the model evaluation measures calculated by the \code{\link{threshMeasures}} function, the area under the ROC curve (AUC) assesses the discrimination performance of a model; but unlike them, it does not require the choice of a particular threshold above which to consider that a model predicts species presence, but rather averages discrimination performance over all possible thresholds. Mind that the AUC has been widely criticized (e.g. Lobo et al. 2008, Jimenez-Valverde et al. 2013), yet it is still among the most widely used metrics in model evaluation. It is highly correlated with species prevalence, so this value is also provided by the AUC function (if \code{simplif = FALSE}, the default) for reference. Although there are functions to calculate the AUC in other R packages (e.g. \pkg{ROCR}, \pkg{PresenceAbsence}, \pkg{verification}, \pkg{Epi}), the \code{AUC} function is more compatible with the remaining functions in \pkg{modEvA} and can be applied not only to a set of observed versus predicted values, but also directly to a model object of class \code{"glm"}.
}
\value{
If \code{simplif = TRUE}, the function returns only the AUC value (a numeric value between 0 and 1). Otherwise (the default), it plots the curve and returns a \code{list} with the following components:
\item{thresholds}{a data frame of the true and false positives, the sensitivity and specificity of the predictions, and the number of predicted values at each analysed threshold.}
\item{N}{the total number of obervations.}
\item{prevalence}{the proportion of occurrences in the data (which correlates with the AUC).}
\item{AUC}{the value of the AUC).}
\item{AUCratio}{the ratio of the obtained AUC value to the null expectation (0.5).}
}
\references{
Lobo, J.M., Jimenez-Valverde, A. & Real, R. (2008). AUC: a misleading measure of the performance of predictive distribution models. Global Ecology and Biogeography 17: 145-151

Jimenez-Valverde, A., Acevedo, P., Barbosa, A.M., Lobo, J.M. & Real, R. (2013). Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]


AUC(model = mod, simplif = TRUE)

AUC(model = mod)

AUC(model = mod, grid = TRUE, plot.preds = TRUE)


# you can also use AUC with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values

AUC(obs = presabs, pred = prediction)
}
