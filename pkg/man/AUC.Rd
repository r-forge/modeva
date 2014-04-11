\name{AUC}
\alias{AUC}
\title{
Area Under the ROC Curve
}
\description{
This function calculates the Area Under the receiver operating characteristic (ROC) Curve for either a model object of class \code{"glm"}, or two matching vectors of observed (binary, 1 for occurrence vs. 0 for non-occurrence) and predicted (continuous, e.g. occurrence probability) values, respectively. The AUC is a measure of the overall discrimination power of the predictions, or the probability that an occurrence site has a higher predicted value than a non-ocurrence site.
}
\usage{
AUC(obs = NULL, pred = NULL, model = NULL, interval = 0.01, simplif = FALSE, plot = TRUE, plot.values = TRUE, xlab = c("False positive rate", "(1-specificity)"), ylab = c("True positive rate", "(sensitivity)"), main = "ROC curve", ...)
}
\arguments{
  \item{obs}{
a vector of observed presences (1) and absences (0) or another binary response variable.
}
  \item{pred}{
a vector with the corresponding predicted values of occurrence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as \code{obs}.
}
  \item{model}{
instead of (and overriding) \code{obs} and \code{pred}, you can provide a model object of class "glm".
}
  \item{interval}{
interval at which to calculate the true positive:false positive ratio. Defaults to 0.01.
}
  \item{simplif}{
logical, whether to use a faster version that outputs only the AUC value (may be useful for calculating AUC for large numbers of species in a loop, for example). Note that the ROC plot is not drawn when simplif = TRUE.
}
  \item{plot}{
logical, whether or not to plot the curve.
}
  \item{plot.values}{
logical, whether or not to show in the plot some values associated to the curve.
}
  \item{xlab}{
label for the x axis.
}
  \item{ylab}{
label for the y axis.
}
  \item{main}{
title for the plot.
}
  \item{\dots}{
further arguments to be passed to the \code{\link{plot}} function.
}
}
\details{
Like the model evaluation measures calculated by the \code{\link{threshMeasures}} function, the area under the ROC curve (AUC) assesses the discrimination performance of a model; but unlike them, it does not require the choice of a particular threshold above which to consider that a model predicts species presence, but rather averages discrimination performance over all possible thresholds at a given interval. Mind that there has been criticism to the AUC (Lobo et al. 2008, Jimenez-Valverde et al. 2013), although it is still one of the most widely used metrics in model evaluation. It is highly correlated with species prevalence, so this value is also provided by the AUC function (if \code{simplif = FALSE}, the default) for reference.

Although there are functions to calculate the AUC in other R packages (such as \code{\pkg{ROCR}}, \code{\pkg{PresenceAbsence}} and \code{\pkg{verification}}), the AUC function is more compatible with the remaining functions in the \code{modEvA} package and can be applied not only to a set of observed versus predicted values, but also directly to a model object of class \code{"glm"}.
}
\value{
If \code{simplif = TRUE}, the function returns only the AUC value (a numeric value between 0 and 1). Otherwise (the default), it plots the curve and returns a \code{list} with the following components:
\item{thresholds}{a data frame of the true and false positives, as well as the sensitivity and specificity of the predictions, at each analysed threshold.}
\item{N}{the total number of obervations.}
\item{prevalence}{the proportion of occurrences in the data (which affects the AUC).}
\item{AUC}{the value of the AUC).}
}
\references{
Lobo, J.M., Jimenez-Valverde, A. & Real, R. (2008). AUC: a misleading measure of the performance of predictive distribution models. Global Ecology and Biogeography 17: 145-151

Jimenez-Valverde, A., Acevedo, P., Barbosa, A.M., Lobo, J.M. & Real, R. (2013). Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + AltitudeRange + HabitatDiversity + HumanPopulation, family = binomial))

AUC(model = mod, simplif = TRUE)

AUC(model = mod)


# you can also use AUC with vectors of observed and predicted values
# instead of with a model object:

prob <- predict(mod, data = rotif.env, type = "response")

AUC(obs = rotif.env$Abrigh, pred = prob)
}
\keyword{ model evaluation }
\keyword{ model performance }
\keyword{ discrimination }