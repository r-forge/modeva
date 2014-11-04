\name{RsqGLM}
\alias{RsqGLM}
\title{R-squared measures for GLMs}
\description{
This function calculates some (pseudo) R-squared statistics for binomial Generalized Linear Models.
}
\usage{
RsqGLM(obs = NULL, pred = NULL, model = NULL)
}
\arguments{
  \item{obs}{a vector of observed presences (1) and absences (0) or another 
  binary response variable.}
  \item{pred}{a vector with the corresponding predicted values of presence probability. Must be of the same length and in the same order as \code{obs}.}
  \item{model}{instead of (and overriding) \code{obs} and \code{pred}, you can provide a model object of class "glm" and family "binomial".}
}
\details{
Implemented measures include the R-squareds of McFadden (1974), Cox-Snell (1989), Nagelkerke (1991, which corresponds to the corrected Cox-Snell, eliminating its upper bound), and Tjur (2009). See Allison (2014) for a brief review of these measures.
}
\value{The function returns a named list of the calculated R-squared values.}
\references{
Allison P. (2014) Measures of fit for logistic regression. SAS Global Forum, Paper 1485-2014

Cox, D.R. & Snell E.J. (1989) The Analysis of Binary Data, 2nd ed. Chapman and Hall, London

McFadden, D. (1974) Conditional logit analysis of qualitative choice behavior. In: Zarembka P. (ed.) Frontiers in Economics. Academic Press, New York

Nagelkerke, N.J.D. (1991) A note on a general definition of the coefficient of determination. Biometrika, 78: 691-692

Tjur T. (2009) Coefficients of determination in logistic regression models - a new proposal: the coefficient of discrimination. The American Statistician, 63: 366-372.
}
\author{A. Marcia Barbosa}
\note{
}
\seealso{\code{\link{AUC}}, \code{\link{threshMeasures}}, \code{\link{HLfit}}}
\examples{
data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + AltitudeRange + 
HabitatDiversity + HumanPopulation, family = binomial))

RsqGLM(model = mod)


# you can also use RsqGLM with vectors of observed and predicted values
# instead of a model object:

prob <- predict(mod, data = rotif.env, type = "response")

RsqGLM(obs = rotif.env$Abrigh, pred = prob)
}
\keyword{ model evaluation }
\keyword{ predictive performance }

