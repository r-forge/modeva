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
logical, whether all bins should have (approximally) the same size.
}
  \item{min.bin.size}{
integer value defining the minimum number of observations to include in each bin. The default is 15, the minimum required for accurate comparisons within bins (Jovani & Tella 2006, Jimenez-Valverde et al. 2013).
}
  \item{min.prob.interval}{
minimum range of probability values in each bin. The default is 0.1.
}
  \item{simplif}{
logical, whether to calculate a faster, simplified version (used internally in other functions). The default is FALSE.
}
}
\details{
Mind that different \code{bin.method}s can lead to visibly different results regarding the bins and any operations that depend on them (such as \code{\link{HLfit}}). Currently available \code{bin.method}s are:

  - \code{round.prob}: probability values are rounded to the number of digits of \code{min.prob.interval} - e.g., if min.prob.interval = 0.1 (the default), values under 0.05 get into bin 1 (rounded probability = 0), values between 0.05 and 0.15 get into bin 2  (rounded probability = 0.1), etc. until values with probability over 0.95, which get into bin 11. Arguments \code{n.bins}, \code{fixed.bin.size} and \code{min.bin.size} are ignored by this bin.method.
  
  - \code{prob.bins}: probability values are grouped into bins of the given probability intervals - e.g., if min.prob.interval = 0.1 (the default), bin 1 gets the values between 0 and 0.1, bin 2 gets the values between 0.1 and 0.2, etc. until bin 10 which gets the values between 0.9 and 1. Arguments \code{n.bins}, \code{fixed.bin.size} and \code{min.bin.size} are ignored by this bin.method.
  
  - \code{size.bins}: probability values are grouped into bins of (approximately) equal size, defined by argument \code{min.bin.size}. Arguments \code{n.bins} and \code{min.prob.interval} are ignored by this bin.method.
  
  - \code{n.bins}: probability values are divided into the number of bins given by argument \code{n.bins}, and their sizes may or may not be forced to be (approximately) equal, depending on argument \code{fixed.bin.size} (which is FALSE by default). Arguments \code{min.bin.size} and \code{min.prob.interval} are ignored by this bin.method.
  
  - \code{quantiles}: probability values are divided using R function \code{\link{quantile}}, with \code{probs} defined by the given \code{min.prob.interval} (i.e., 0.1 intervals by default). Arguments \code{n.bins}, \code{fixed.bin.size} and \code{min.bin.size} are ignored by this bin.method.
}
\value{
The output of \code{getBins} is a list with the following components:
\item{prob.bin}{the first and last value of each bin}
\item{bins.table}{a data frame with the numbers of presences, absences, prevalence, mean probability, etc. in each bin.}
\item{N}{the total number of observations in the analysis.}
\item{n.bins}{the total number of bins obtained.}
}
\references{
Jimenez-Valverde A., Acevedo P., Barbosa A.M., Lobo J.M. & Real R. (2013) Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508–516.

Jovani R. & Tella J.L. (2006) Parasite prevalence and sample size: misconceptions and solutions. Trends in Parasitology 22: 214-218.
}
\author{
A. Marcia Barbosa
}
\note{
This function is still under development and may fail for some datasets and binning methods (e.g. ties may sometimes preclude binning under some bin.methods). Fixes and further methods are in preparation. Feedback is welcome.
}
\seealso{
\code{\link{HLfit}}}
\examples{
# load sample models:

data(rotif.mods)


# choose a particular model to play with:

mod <- rotif.mods$models[[1]]


# try getBins using different binning methods:

getBins(model = mod, bin.method = "quantiles")

getBins(model = mod, bin.method = "n.bins")

getBins(model = mod, bin.method = "n.bins", fixed.bin.size = TRUE)
}
\keyword{ }
