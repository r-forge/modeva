\name{plotGLM}
\alias{plotGLM}
\title{
Plot a generalized linear model
}
\description{
This function plots the observed (presence/absence) data and the predicted (probability) values of a generalized linear model against the y regression equation values. For now, it works only for logistic regression (binomial response, logit link). Optionally (if \code{plot.values = TRUE}) it can display in the plot the proportion of deviance explained (Weisberg 1980, Guisan & Zimmermann 2000), but this requires that a model object is provided (rather than just the observed and predicted values).
}
\usage{
plotGLM(obs = NULL, pred = NULL, model = NULL, link = "logit", plot.values = FALSE, 
xlab = "Logit (y)", ylab = "Predicted probability", main = "Model plot", ...)
}
\arguments{
  \item{obs}{a vector of presence/absence or other binary (1-0) observed data.}
  \item{pred}{a vector of the values predicted by a GLM of the binary observed data.}
  \item{model}{instead of (and overriding) \code{obs} and \code{pred}, you can provide a model object of class "glm".}
  \item{link}{the link function of the GLM; only 'logit' (the default) is implemented.}
  \item{plot.values}{logical, whether to report the values of deviance and adjusted deviance in the plot (requires a \code{model} argument).}
  \item{xlab}{character indicating the label for the x axis.}
  \item{ylab}{character indicating the label for the y axis.}
  \item{main}{character indicating the title for the plot.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\details{
}
\value{
This function outputs a plot of the provided model.
}
\references{
Guisan A. & Zimmermann N.E. (2000) Predictive habitat distribution models in ecology. Ecological Modelling 135: 147-186

Weisberg S. (1980) Applied Linear Regression. Wiley, New York
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{glm}}, \code{\link{Dsquared}}
}
\examples{
data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + AltitudeRange + 
HabitatDiversity + HumanPopulation, family = binomial))

plotGLM(model = mod)

plotGLM(model = mod, plot.values = TRUE)


# you can also use \code{plotGLM} with vectors of observed and predicted values
# instead of with a model object:

prob <- predict(mod, data = rotif.env, type = "response")

plotGLM(obs = rotif.env$Abrigh, pred = prob)
}
\keyword{ models }
\keyword{ regression }
