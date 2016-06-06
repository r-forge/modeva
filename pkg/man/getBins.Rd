\name{getBins}
\alias{getBins}
\title{
Get bins of data.
}
\description{
Get continuous predicted values into bins according to specific criteria.
}
\usage{
getBins(model = NULL, obs = NULL, pred = NULL, id = NULL, bin.method,
n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15, min.prob.interval = 0.1,
simplif = FALSE)
}
\arguments{
  \item{model}{a model object of class \code{"glm"}.
}
  \item{obs}{
a vector of 1-0 values of a modelled binary variable. Not necessary (and ignored) if \code{model} is provided.
}
  \item{pred}{
a vector of the corresponding predicted values. Not necessary (and ignored) if \code{model} is provided.
  }
  \item{id}{
optional vector of row identifiers; must be of the same length and in the same order of \code{obs} and \code{pred} (or of the cases used to build \code{model})
}
  \item{bin.method}{
the method with which to divide the values into bins. Type \code{modEvAmethods("getBins")} for available options and see Details for more information on these methods.
}
  \item{n.bins}{
the number of bins in which to divide the data.
}
  \item{fixed.bin.size}{
logical, whether all bins should have the same size.
}
  \item{min.bin.size}{
integer value defining the minimum number of observations to include in each bin. The default minimum is 15, following Jovani & Tella (2006).
}
  \item{min.prob.interval}{
minimum range of probability values in each bin. Defaults to 0.1.
}
  \item{simplif}{
logical, whether to calculate a simplified faster version (usd internally in other functions). The default is FALSE.
}
}
\details{
Different \code{bin.method}s can lead to visibly different results, regarding the bins and other operations that depend on them (such as \code{\link{HLfit}}). Currently available \code{bin.method}s are:

  - \code{round.prob}: probability values are rounded (e.g., those under 0.5 get into bin 1 (rounded probability = 0), those between 0.5 and 1.5 get into bin 2  (rounded probability = 1), etc. until those values with probability over 0.95, which get into bin 11).
  
  - \code{prob.bins}: probability values are grouped into bins of the given probability intervals (e.g., if \code{min.prob.interval = 0.1}, bin 1 gets the values between 0 and 0.1, bin 2 gets the values between 0.1 and 0.2, etc. until bin 10 which gets the values between 0.9 and 1).
  
  - \code{size.bins}: probability values are grouped into bins of (approximately) equal size, defined by argument \code{min.bin.size}.
  
  - \code{n.bins}: probability values are divided into the number of bins given by argument \code{n.bins}, and their sizes can be fixed or not depending on argument \code{fixed.bin.size} (which is FALSE by default).
  
  - \code{quantiles}: probability values are divided using R function \code{\link{quantile}}.
  
This function is still under development and may fail for some datasets and binning methods (e.g. ties may sometimes preclude some binning methods). Fixes and further methods are in preparation.
}
\value{
The output of \code{getBins} is a list with the following components:
\item{prob.bin}{the first and last value of each bin}
\item{bins.table}{a data frame with the numbers of presences, absences, prevalence, mean probability, etc. in each bin.}
\item{N}{the total number of observations in the analysis.}
\item{n.bins}{the total number of bins obtained.}
}
\references{
Jovani R. & Tella J.L. (2006) Parasite prevalence and sample size: misconceptions and solutions. Trends in Parasitology 22: 214-218.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{HLfit}}}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

getBins(model = mod, bin.method = "quantiles")

}
\keyword{ }
