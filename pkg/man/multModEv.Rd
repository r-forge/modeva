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
bin.method = NULL, quiet = TRUE, ...)
}
\arguments{
  \item{models}{a \code{list} of model object(s) of class "glm", all applied to the same data set.}
  \item{obs.data}{a data frame with observed binary data. Not necessary (and ignored) if \code{models} is provided.}
  \item{pred.data}{a data frame with the corresponding predicted values, with both rows and columns in the same order as in \code{obs.data}. Not necessary (and ignored) if \code{models} is provided. Note that, for calibration measures (based on \code{\link{HLfit}} or \code{\link{MillerCalib}}), the results are only valid if the input predictions represent probability.}
  \item{measures}{character vector of the evaluation measures to calculate. The default is all implemented measures, which you can check by typing \code{modEvAmethods("multModEv")}. But beware: calibration measures (i.e., HL and Miller) are only valid if your predicted values reflect actual presence probability (not favourability, habitat suitability or others); you should exclude them otherwise.}
  \item{standardize}{logical, whether to standardize measures that vary between -1 and 1 to the 0-1 scale (see \code{\link{standard01}}).}
  \item{thresh}{the threshold value to use for calculating threshold-based measures (the ones in \code{modEvAmethods("threshMeasures")}). Can be "preval" (for species prevalence) or any number between 0 and 1. The default is 0.5.}
  \item{bin.method}{the method with which to divide the data into groups or bins (for calibration or reliability measures such as \code{\link{HLfit}}). The default is NULL, but a valid method must be specified if \code{measures} includes "HL" or "HL.p". Type modEvAmethods("getBins") for available options), and see \code{\link{HLfit}} and \code{\link{getBins}} for more information.}
  \item{quiet}{logical, whether to be quiet or display messages.}
  \item{\dots}{optional arguments to pass to \code{\link{HLfit}}, namely \code{n.bins}, \code{fixed.bin.size}, \code{min.bin.size} or \code{min.prob.interval}.}
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

eval1 <- multModEv(models = rotif.mods$models[1:6], thresh = 0.5, 
bin.method = "n.bins", fixed.bin.size = TRUE)

head(eval1)


eval2 <- multModEv(models = rotif.mods$models[1:6], thresh = "preval", 
measures = c("AUC", "CCR", "Sensitivity", "TSS"))

head(eval2)


# you can also calculate evaluation measures for a set of 
# observed vs predicted data, rather than from model objects:

obses <- sapply(rotif.mods$models, `[[`, "y")
preds <- sapply(rotif.mods$models, `[[`, "fitted.values")

eval3 <- multModEv(obs.data = obses[ , 1:4], pred.data = preds[ , 1:4], 
thresh = "preval", bin.method = "prob.bins")

head(eval3)
}
\keyword{ }
