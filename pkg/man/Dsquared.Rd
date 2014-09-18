\name{Dsquared}
\alias{Dsquared}
\title{
Proportion of deviance explained by a GLM
}
\description{
\code{Dsquared} calculates the (adjusted) amount of deviance accounted for by a generalized linear model.
}
\usage{
Dsquared(model, adjust = FALSE)
}
\arguments{
  \item{model}{
a model object of class \code{"glm"}.
}
  \item{adjust}{
logical, whether or not to adjust the amount of explained deviance according to the number of observations and parameters in the model (see Details).
}
}
\details{
Linear models come with an R-squared value that measures the proportion of variation that the model accounts for. The R-squared is provided with \code{summary(model)} in R. For generalized linear models (GLMs), the equivalent is the amount of deviance accounted for (D-squared; Guisan & Zimmermann 2000), but this value is not normally provided with the model summary. The \code{Dsquared} function calculates it. There is also an option to calculate the adjusted D-squared, which takes into account the number of observations and the number of predictors, thus allowing direct comparison among different models (Weisberg 1980, Guisan & Zimmermann 2000).
}
\value{
a numeric value indicating the proportion of deviance accounted for by \code{model}.
}
\references{
Guisan, A. & Zimmermann, N.E. (2000) Predictive habitat distribution models in ecology. Ecological Modelling 135: 147-186

Weisberg, S. (1980) Applied Linear Regression. Wiley, New York
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{glm}}, \code{\link{plotGLM}}
}
\examples{
data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + AltitudeRange + 
HabitatDiversity + HumanPopulation, family = binomial))

Dsquared(mod)

Dsquared(mod, adjust = TRUE)
}
\keyword{ models }
