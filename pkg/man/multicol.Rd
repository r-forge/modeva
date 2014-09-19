\name{multicol}
\alias{multicol}
\title{Analyse the multicollinearity in a dataset, including tolerance and VIF}
\description{
This function analyses the multicollinearity in a dataset, including the R-squared, tolerance and variance inflation factor (VIF).
}
\usage{
multicol(data, var.cols = 1:ncol(data))
}
\arguments{
  \item{data}{
A matrix or data frame containing the numeric variables for which to calculate multicollinearity.
}
  \item{var.cols}{
Index numbers of the columns in \code{data} containing the numeric variables to analyse multicollinearity for.
}
}
\details{
The multicol function calculates the degree of multicollinearity in a set of 
numeric variables, using three closely related measures: R-squared, tolerance and 
the variance inflation factor (VIF). All three measures are calculated for each 
covariate based on the coefficient of determination (R-squared) of a linear
regression in which that covariate is used as the response and all other 
covariates are set as explanatory variables (Marquardt 1970; Mansfield & Helms 
1982). Such measures (especially the VIF) are widely used in ecological modelling 
and a range of other research areas, and several packages already calculate the 
VIF in R, but their results can be strikingly different (Estrada & Barbosa, 
submitted). The \code{multicol} function calculates multicollinearity statistics 
directly on the independent variables, producing the expected results and without 
(so far) clashing with other packages.
}
\value{
The function returns a matrix with one row per analysed variable, the names of 
the variables as row names, and 3 columns: R-squared, Tolerance and VIF.
}
\references{
Estrada A. & Barbosa A.M. (submitted) Calculating the Variance Inflation Factor 
in R: cautions and recommendations.

Marquardt D.W. (1970) Generalized inverses, ridge regression, biased linear 
estimation, and nonlinear estimation. Technometrics 12: 591-612.

Mansfield E.R. & Helms B.P. (1982) Detecting multicollinearity. The American 
Statistician 36: 158-160.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
}
\examples{
multicol(trees)
 
# you'll get a warning and some NA results if any of the variables is not numeric:
multicol(OrchardSprays)
 
# but you can define a subset of columns of 'data' to calculate 'multicol' for:
multicol(OrchardSprays, var.cols = 1:3)
}
\keyword{collinearity}
\keyword{tolerance}
\keyword{VIF}