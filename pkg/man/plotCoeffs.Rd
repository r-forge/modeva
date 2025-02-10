\name{plotCoeffs}
\alias{plotCoeffs}
\title{Plot model coefficients with confidence intervals}
\description{
  Plots the coefficients of a model with their 95\% confidence intervals, using a point range plot.
}
\usage{
  plotCoeffs(model, labels = NULL, ...)
}
\arguments{
  \item{model}{A model object of class "glm", "lm" or "phylolm".}
  \item{labels}{Optional two-column matrix or data frame, with the first column containing names of variables that appear in 'model' (in no particular order), and the second column containing the corresponding names to use as labels in the plot. Can be useful when the model contains abbreviated variable names, and you want more complete variable names in the plot.}
  \item{...}{Additional arguments to pass to \code{\link{lollipop}}, e.g. horizontal = TRUE.}
}
\details{
  This function uses \code{\link{lollipop}} to draw a point range plot of the coefficients of a fitted model (excluding the intercept) with their 95\% confidence intervals (computed from the standard error in the model summary). The labels of coefficients with significant p-values (under alpha = 0.05) are displayed in bold. Note that coefficients are only directly comparable if the variables were standardized or if they vary in the same scale.
}
\value{
  A plot.
}
\author{
  A. Marcia Barbosa
}
\seealso{
  \code{\link{varImp}}, \code{\link{lollipop}}
}
\examples{
# get example model:

mod <- glm(am ~ mpg + wt + gear + carb, data = as.data.frame(scale(mtcars)))

summary(mod)


# default plot:

plotCoeffs(mod)


# with some tweaks:

vars <- c("gear", "wt", "mpg", "carb")
labs <- c("Number of gears", "Weight", "Miles par gallon", "Number of carburators")

par(mar = c(3, 10, 2, 1))

plotCoeffs(mod, horiz = TRUE, cex = 2, grid = FALSE, axis.lab = "Coefficients", 
main = "Transmission", labels = data.frame(vars, labs), col = "purple")
}
