\name{plotGLM}
\alias{plotGLM}
\title{
Plot a generalized linear model
}
\description{
This function plots the observed (presence/absence) data and the predicted (probability) values of a Generalized Linear Model against the y regression equation (logit) values. Only logistic regression (binomial response, logit link) is implemented. Optionally (if \code{plot.values = TRUE}) it can display in the plot the proportion of deviance explained (Weisberg 1980, Guisan & Zimmermann 2000).
}
\usage{
plotGLM(obs = NULL, pred = NULL, model = NULL, link = "logit", plot.values = FALSE, 
xlab = "Logit (Y)", ylab = "Predicted probability", main = "Model plot", ...)
}
\arguments{
  \item{obs}{a vector of presence/absence or other binary (1-0) observed data.}
  \item{pred}{a vector of the values predicted by a GLM of the binary observed data.}
  \item{model}{instead of (and overriding) \code{obs} and \code{pred}, a model object of class "glm".}
  \item{link}{the link function of the GLM; only 'logit' (the default) is implemented.}
  \item{plot.values}{logical, whether to report values associated with the model in the plot (see Details). Defaults to \code{FALSE}.}
  \item{xlab}{character string specifying the label for the x axis.}
  \item{ylab}{character string specifying the label for the y axis.}
  \item{main}{character string specifying the title for the plot.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\details{
If \code{plot.values = TRUE}, the plot will also display values such as deviance (and adjusted deviance if \code{model} is provided). Deviance is calculated with the \code{\link{Dsquared}} function. Additional measures will be implemented in the future.
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

prob <- predict(object = mod, data = rotif.env, type = "response")

plotGLM(obs = rotif.env$Abrigh, pred = prob)
}
\keyword{ models }
\keyword{ regression }
