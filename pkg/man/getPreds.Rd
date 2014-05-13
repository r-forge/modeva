\name{getPreds}
\alias{getPreds}
\title{
Get model predictions
}
\description{
This function allows getting the predictions of multiple models when applied to a new dataset. It can be useful if you have a list of model objects (e.g. resulting from \code{\link{multGLM}}) and want to apply them to a new data set (e.g. with variables from another region or time period), with options to include the logit link (y) and/or Favourability (see \code{\link{Fav}}), in just one step.
}
\usage{
getPreds(data, models, y = FALSE, P = TRUE, Favourability = FALSE)
}
\arguments{
  \item{data}{
the data frame to which to add model predictions; must contain all variables (with the same names, case-sensitive) included in the \code{models}.
}
  \item{models}{
a list of model objects obtained e.g. with function \code{\link{glm}} or \code{\link{multGLM}}.
}
  \item{y}{
logical, whether to include the logit link (y) value in the predictions.
}
  \item{P}{
logical, whether to include the probability value in the predictions.
}
  \item{Favourability}{
logical, whether to include Favourability in the predictions (see \code{\link{Fav}}).
}
}
\details{
}
\value{
This functions returns a data frame containing the input \code{data} plus some new columns with the predictions.
}
\references{
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{multGLM}}, \code{\link{predict}}
}
\examples{
data(rotif.env)

names(rotif.env)


# identify rotifer data corresponding to the Eastern and Western hemispheres:

unique(rotif.env$CONTINENT)

rotif.env$HEMISPHERE <- "Eastern"

rotif.env$HEMISPHERE[rotif.env$CONTINENT \%in\% 
c("NORTHERN_AMERICA", "SOUTHERN_AMERICA")] <- "Western"

head(rotif.env)


# separate the rotifer data into hemispheres

east.hem <- rotif.env[rotif.env$HEMISPHERE == "Eastern", ]
west.hem <- rotif.env[rotif.env$HEMISPHERE == "Western", ]


# make models for 3 of the species in rotif.env based on their distribution 
# in the Eastern hemisphere:

mods <- multGLM(east.hem, sp.cols = 18:20, var.cols = 5:17, id.col = 1, 
step = FALSE, FDR = FALSE, trim = FALSE)


# get the models' predictions for the Western hemisphere dataset:

preds <- getPreds(west.hem, models = mods$models, P = TRUE, Favourability = TRUE)

head(preds)

}
\keyword{ model }
\keyword{ prediction }
