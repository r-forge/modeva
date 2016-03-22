\name{multModEv}
\alias{multModEv}
\title{
Multiple model evaluation
}
\description{
If you have a list of GLM model objects (created, e.g., with the \code{multGLM} function of the \pkg{fuzzySim} package), or a data frame with presence-absence data and the corresponding predicted values for a set of species, you can use the \code{multModEv} function to get a set of evaluation measures for all models simultaneously, as long as they all have the same sample size.
}
\usage{
multModEv(models = NULL, obs.data = NULL, pred.data = NULL,
measures = modEvAmethods("multModEv"), standardize = FALSE, thresh = 0.5,
bin.method = "quantiles", quiet = TRUE)
}
\arguments{
  \item{models}{a \code{list} of model object(s) of class "glm", all applied to the same data set.}
  \item{obs.data}{a data frame with observed binary data. Not necessary (and ignored) if \code{models} is provided.}
  \item{pred.data}{a data frame with the corresponding predicted values, with both rows and columns in the same order as in \code{obs.data}. Not necessary (and ignored) if \code{models} is provided. Note that, for calibration measures (based on \code{\link{HLfit}} or \code{\link{MillerCalib}}), the results are only valid if the input predictions represent presence probability.}
  \item{measures}{character vector of the evaluation measures to calculate. The default is all implemented measures, which you can check by typing \code{modEvAmethods("multModEv")}.}
  \item{standardize}{logical, whether to standardize measures that vary between -1 and 1 to the 0-1 scale (see \code{\link{standard01}}).}
  \item{thresh}{the threshold value to use for calculating threshold-based measures (the ones in \code{modEvAmethods("threshMeasures")}). Can be "preval" (for species prevalence) or any number between 0 and 1. The default is 0.5.}
  \item{bin.method}{the method with which to divide the data into groups or bins (for calibration or reliability measures such as \code{\link{HLfit}}); type modEvAmethods("getBins") for available options).}
  \item{quiet}{logical, whether to be quiet or display messages.}
}
\details{
}
\value{
A data frame with the value of each evaluation measure for each model.
}
\references{
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
data(rotif.mods)

eval <- multModEv(models = rotif.mods$models[1:6], thresh = "preval",
measures = c("AUC", "CCR", "Sensitivity", "TSS", "HL.p"))

head(eval)


# you can also calculate evaluation measures for a set of 
# observed vs predicted data, rather than from model objects:

\dontrun{
require(fuzzySim)  # currently available on R-Forge
data(rotif.env)

eval <- multModEv(obs.data = rotif.env[ , 18:20],
pred.data = rotif.mods$predictions[ , c("Abrigh_P", "Afissa_P", "Apriod_P")],
thresh = "preval")

head(eval)
}

}
\keyword{ }
