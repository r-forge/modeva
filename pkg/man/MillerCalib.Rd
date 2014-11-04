\name{MillerCalib}
\alias{MillerCalib}
\title{Miller's calibration satistics for logistic regression models}
\description{
This function calculates Miller's (1991) calibration statistics for a generalized linear model with binomial distribution and logistic link, namely the intercept and slope of the regression of the response variable on the logit of predicted probabilities. Optionally and by default, it also plots the corresponding regression line (black) over the reference diagonal (grey).
}
\usage{
MillerCalib(obs = NULL, pred = NULL, model = NULL, plot = TRUE, plot.values = TRUE, digits = 4, xlab = "", ylab = "", main = "Miller calibration", ...)
}
\arguments{
  \item{obs}{a vector of observed presences (1) and absences (0) or another 
  binary response variable.}
  \item{pred}{a vector with the corresponding predicted values of presence probability. Must be of the same length and in the same order as \code{obs}.}
  \item{model}{instead of (and overriding) \code{obs} and \code{pred}, you can provide a model object of class "glm" and family "binomial".}
  \item{plot}{logical, whether to plot the regression line over the reference diagonal. Defaults to TRUE.}
  \item{plot.values}{logical value indicating whether to report the statistics in the plot. Defaults to TRUE.}
  \item{digits}{integer number indicating the number of digits to which the values in the plot should be rounded. Dafaults to 4. Ignored if \code{plot} or \code{plot.values} are set to \code{FALSE}.}
  \item{xlab}{label for the x axis.}
  \item{ylab}{label for the y axis.}
  \item{main}{title for the plot.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\details{
Calibration or reliability measures how a model's predicted probabilities relate to observed species prevalence or proportion of presences in the modelled data (Pearce & Ferrier 2000; Wintle et al. 2005; Franklin 2010). If predictions are perfectly calibrated, the slope will equal 1 and the intercept will equal 0, so the model's calibation line will perfectly overlap with the reference diagonal. Note that Miller's statistics assess the model globally: a model is well calibrated if the average of all predicted probabilities equals the proportion of presences in the modelled data. Good calibration is always attained on the same data used for building the model (Miller 1991).
}
\value{
This function returns a list of two integer values:
\item{intercept}{the calibration intercept.}
\item{slope}{the calibration slope.}
If \code{plot = TRUE}, a plot will be produced with the calibration line (black) and the reference diagonal (dashed grey).
}
\references{
Franklin, J. (2010) Mapping Species Distributions: Spatial Inference and Prediction. Cambridge University Press, Cambridge.

Miller M.E., Hui S.L. & Tierney W.M. (1991) Validation techniques for logistic regression models. Statistics in Medicine, 10: 1213-1226

Pearce J. & Ferrier S. (2000) Evaluating the predictive performance of habitat models developed using logistic regression. Ecological Modelling, 133: 225-245

Wintle B.A., Elith J. & Potts J.M. (2005) Fauna habitat modelling and mapping: A review and case study in the Lower Hunter Central Coast region of NSW. Austral Ecology, 30: 719-738
}
\author{A. Marcia Barbosa}
\note{
}
\seealso{
\code{\link{HLfit}}, \code{\link{RsqGLM}}
}
\examples{
data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + AltitudeRange + 
HabitatDiversity + HumanPopulation, family = binomial))

MillerCalib(model = mod)


# you can also use MillerCalib with vectors of observed and predicted values
# instead of a model object:

prob <- predict(mod, data = rotif.env, type = "response")

MillerCalib(obs = rotif.env$Abrigh, pred = prob)
}
\keyword{ model evaluation }
\keyword{ model calibration }