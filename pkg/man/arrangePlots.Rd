\name{arrangePlots}
\alias{arrangePlots}
\title{
Arrange plots
}
\description{
Get an appropriate row/column combination (for \code{par(mfrow)}) for arranging a given number of plots within a plotting window.
}
\usage{
arrangePlots(n.plots, landscape = FALSE)
}
\arguments{
  \item{n.plots}{
number of plots to be placed in the graphics device.
}
  \item{landscape}{
logical, whether the plotting window should be landscape/horizontal (number of columns larger than the number of rows) or not. The value does not make a difference if the number of plots makes for a square plotting window.
}
}
\details{
This function is used internally by \code{optiThresh}, but can also be useful outside it.
}
\value{
An integer vector of the form c(nr, nc) indicating, respectively, the number of rows and of columns of plots to set in the graphics device. 
}
\references{
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{plot}}, \code{\link{layout}}
}
\examples{
arrangePlots(10)

arrangePlots(10, landscape = TRUE)


# a more practical example:

data(rotif.env)

names(rotif.env)

# say you want to plot columns 5 to 9 in a nicely arranged plotting window:

par(mfrow = arrangePlots(length(5:9)))

for (i in 5:9) {
  plot(1:nrow(rotif.env), rotif.env[, i])
}
}
\keyword{ plot }
\keyword{ graphics }
