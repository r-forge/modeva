\name{rotif.mods}
\alias{rotif.mods}
\docType{data}
\title{Rotifer distribution models}
\description{
A set of generalized linear models of rotifer species distributions on TDWG level 4 regions of the world, together with their predicted values. Mind that these models are provided just as sample data and have limited application, due to limtations in the underlying distribution records. See Details for more information.
}
\usage{data(rotif.mods)}
\format{
A list of 2 elements:
 $ predictions: a data.frame with  291 observations of 60 variables, namely the presence probability (P) and environmental favourability (F) for each of 30 species of rotifers, obtained from the rotif.env dataset in the \pkg{fuzzySim} package
 $ models: a list of the 30 generalized linear model (\code{link{glm}}) objects which generated those predictions.
}
\details{
These models were obtained with the \code{multGLM} function and the \code{rotif.env} dataset from package \pkg{fuzzySim}, using the following command:
\code{mods <- multGLM(data = rotif.env, sp.cols = 18:47, var.cols = 5:17, step = FALSE, trim = TRUE)}
See package \pkg{fuzzySim} (currently available on R-Forge) for more information source data.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Fontaneto D., Barbosa A.M., Segers H. & Pautasso M. (2012) The 'rotiferologist' effect and other global correlates of species richness in monogonont rotifers. Ecography, 35: 174-182.
}
\examples{
data(rotif.mods)
head(rotif.mods$predictions)
rotif.mods$models[[1]]
}
\keyword{datasets}