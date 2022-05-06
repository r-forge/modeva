\name{Dsquared}
\alias{Dsquared}
\title{
Proportion of deviance explained by a GLM
}
\description{
This function calculates the (adjusted) amount of deviance accounted for by a generalized linear model.
}
\usage{
Dsquared(model = NULL, obs = NULL, pred = NULL, family = NULL, 
adjust = FALSE, npar = NULL, na.rm = TRUE, rm.dup = FALSE)
}
\arguments{
  \item{model}{a model object of class "glm". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{family}{a character vector (i.e. in quotes) of length 1 specifying the family of the GLM. This argument is ignored if \code{model} is provided; otherwise (i.e. if 'obs' and 'pred' are provided rather than a model object), only families 'binomial' (logit link) and 'poisson' (log link) are currently implemented.}
  \item{adjust}{logical, whether or not to adjust the D-squared value for the number of observations and parameters in the model (see Details). The default is \code{FALSE}; \code{TRUE} requires either providing the \code{model} object, or specifying the number of parameters in the model that produced the \code{pred} values.}
  \item{npar}{an integer vector indicating the number of parameters in the model. This argument is ignored if \code{model} is provided or if \code{adjust = FALSE}.}
    \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
}
\details{
Linear models come with an R-squared value that measures the proportion of variation that the model accounts for. The R-squared is provided with \code{summary(model)} in R. For generalized linear models (GLMs), the equivalent is the amount of deviance accounted for (D-squared; Guisan & Zimmermann 2000), but this value is not normally provided with the model summary. The \code{Dsquared} function calculates it. There is also an option to calculate the adjusted D-squared, which takes into account the number of observations and the number of predictors, thus allowing direct comparison among different models (Weisberg 1980, Guisan & Zimmermann 2000).
}
\value{
This function returns a numeric value indicating the (adjusted) proportion of deviance accounted for by the model.
}
\references{
Guisan, A. & Zimmermann, N.E. (2000) Predictive habitat distribution models in ecology. Ecological Modelling 135: 147-186

Weisberg, S. (1980) Applied Linear Regression. Wiley, New York
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{glm}}, \code{\link{plotGLM}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

Dsquared(model = mod)

Dsquared(model = mod, adjust = TRUE)


# you can also use Dsquared with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values
parameters <- attributes(logLik(mod))$df

Dsquared(obs = presabs, pred = prediction, family = "binomial")

Dsquared(obs = presabs, pred = prediction, family = "binomial",
adjust = TRUE, npar = parameters)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
