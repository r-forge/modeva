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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a matrix with one row per analysed variable, the names of the variables as row names, and 3 columns: R-squared, Tolerance and VIF.
}
\references{
%% ~put references to the literature/web site here ~
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