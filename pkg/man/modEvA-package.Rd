\name{modEvA-package}
\alias{modEvA-package}
\alias{modEvA}
\docType{package}
\title{
Model Evaluation and Analysis
}
\description{
The modEvA package can build and analyse species distribution models and 
evaluate their performance. Includes functions for performing variation 
partitioning, assessing the false discovery rate, calculating several measures 
of model discrimination and calibration, optimizing prediction thresholds 
based on a number of criteria, performing multivariate environmental similarity 
surface (MESS) analysis, and displaying various analytical plots. Includes also 
a sample data set with species occurrences and predictor variables.
}
\details{
\tabular{ll}{
Package: \tab modEvA\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2014-05-08\cr
License: \tab GPL-3\cr
}
}
\author{
Barbosa A.M., Brown J.A., Jimenez-Valverde A., Real R.

A. Marcia Barbosa <barbosa@uevora.pt>
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing 
model equilibrium and prediction mismatch in species distribution models. 
Diversity and Distributions 19: 1333-1338 (DOI: 10.1111/ddi.12100)
}
\keyword{ package }
\seealso{
\code{\pkg{PresenceAbsence}}, \code{\pkg{ROCR}}, \code{\pkg{verification}}, \code{\pkg{dismo}}
}
\examples{
data(rotif.env)

names(rotif.env)

mods <- multGLM(data = rotif.env, sp.cols = 18:47, var.cols = 5:17, 
step = FALSE, trim = TRUE)

head(mods$predictions)

Dsquared(mods$models[[1]])

plotGLM(model = mods$models[[1]])

AUC(model = mods$models[[1]])

threshMeasures(model = mods$models[[1]], thresh = "preval")

optiThresh(model = mods$models[[1]])

optiPair(model = mods$models[[1]], measures = c("Omission", "Commission"))

HLfit(model = mods$models[[1]])
}
