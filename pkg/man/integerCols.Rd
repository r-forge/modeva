\name{integerCols}
\alias{integerCols}
\title{
Classify integer columns
}
\description{
This function detects which columns in a data frame contain only whole numbers, 
and converts those columns to integer class, so that they take up less space.
}
\usage{
integerCols(data)
}
\arguments{
  \item{data}{a data frame containing possibly integer columns classified as \code{numeric}.}
}
\details{
}
\value{
The function returns a data frame with the same columns as \code{data}, but with 
those that contain only whole numbers (possibly including NA) classified as 
\code{integer}.
}
\references{
}
\author{A. Marcia Barbosa}
\note{
}
\seealso{
\code{\link{is.integer}}, \code{\link{as.integer}}, \code{\link{multConvert}}
}
\examples{
dat <- data.frame(
  a = 1:10,
  b = as.numeric(1:10),
  c = seq(0.1, 1, 0.1),
  d = letters[1:10]
)

str(dat)  # b is classified as 'numeric' although it contains only whole numbers

dat <- integerCols(dat)

str(dat)  # b now classified as 'integer'
}
\keyword{ data management }
\keyword{ conversion }
