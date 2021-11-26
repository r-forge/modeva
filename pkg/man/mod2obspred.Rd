\name{mod2obspred}
\alias{mod2obspred}
\title{
Observed and predicted values from a model object.
}
\description{
This function takes a model object and returns the observed and (optionally) the fitted values in that model.
}
\usage{
mod2obspred(model, obs.only = FALSE)
}
\arguments{
  \item{model}{a model object of class "glm", "gam", "gbm", "randomForest" or "bart" from which the response variable and fitted values can be extracted.}
  \item{obs.only}{logical, whether only 'obs' should be obtained (saves computing time when 'pred' not needed -- used e.g. by \code{\link{prevalence}}). Defaults to FALSE.}
}
\value{
A data frame with one column containing the observed and (if obs.only=FALSE, the default) another column containing the predicted values from 'model'.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{prevalence}}
}
\examples{
data(rotif.mods)
mod <- rotif.mods$models[[1]]
obspred <- mod2obspred(mod)
head(obspred)
}
