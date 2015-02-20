\name{getBins}
\alias{getBins}
\title{
Get bins of data.
}
\description{
Get continuous predicted values into bins according to specific criteria.
}
\usage{
getBins(obs = NULL, pred = NULL, model = NULL, id = NULL, bin.method = "quantiles",
n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15, min.prob.interval = 0.1,
simplif = FALSE)
}
\arguments{
  \item{obs}{
a vector of 1-0 values of a modelled binary variable.
}
  \item{pred}{
a vector of the corresponding predicted values.
  }
  \item{model}{
instead of (and overriding) \code{obs} and \code{pred}, a model object of class \code{"glm"}.
}
  \item{id}{
optional vector of row identifiers; must be of the same length and in the same order of \code{obs} and \code{pred} (or of the cases used to build \code{model})
}
  \item{bin.method}{
the method with which to divide the values into bins. Type \code{modEvAmethods("getBins")} for available options.
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

getBins(model = mod)

}
\keyword{ binning }
