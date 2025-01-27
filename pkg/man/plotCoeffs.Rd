\name{plotCoeffs}
\alias{plotCoeffs}
\title{Plot model coefficients with confidence intervals}
\description{
  Plots the coefficients of a model along with their confidence intervals, using a point range plot.
}
\usage{
  plotCoeffs(model, ...)
}
\arguments{
  \item{model}{A model object of class "glm", "lm" or "phylolm".}
  \item{...}{Additional arguments to pass to \code{\link{lollipop}}, e.g. horizontal = TRUE.}
}
\details{
  This function uses \code{\link{lollipop}} to draw a point range plot of the coefficients of a fitted model (excluding the intercept) with their 95\% confidence intervals. The labels of coefficients with significant p-values (under alpha = 0.05) are displayed in bold.
}
\value{
  A plot.
}
\author{
  A. Marcia Barbosa
}
\examples{
# get example data and model:

dat <- subset(iris, iris$Species \%in\% c("setosa", "versicolor"))

dat$Species <- ifelse(dat$Species == "setosa", 1, 0)

mod <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = dat)


# default lollipop plot:

par(mar = c(6, 3, 2, 1))

plotCoeffs(mod)


# add some tweaks:

par(mar = c(3, 6, 2, 1))

plotCoeffs(mod, horiz = TRUE, grid = FALSE, 
xlab = "Coefficients", main = "Species model")
}
