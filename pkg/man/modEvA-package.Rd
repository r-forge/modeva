\name{modEvA-package}
\alias{modEvA-package}
\alias{modEvA}
\docType{package}
\title{Model Evaluation and Analysis}
\description{
The modEvA package can analyse species distribution models and evaluate their performance. It includes functions for performing variation partitioning, calculating several measures of model discrimination and calibration, optimizing prediction thresholds based on a number of criteria, performing multivariate environmental similarity surface (MESS) analysis, and displaying various analytical plots.
}
\details{
\tabular{ll}{
Package: \tab modEvA\cr
Type: \tab Package\cr
Version: \tab 1.2.3\cr
Date: \tab 2015-12-10\cr
License: \tab GPL-3\cr
}
}
\author{
Barbosa A.M., Brown J.A., Jimenez-Valverde A., Real R.

A. Marcia Barbosa <barbosa@uevora.pt>
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions 19: 1333-1338 (DOI: 10.1111/ddi.12100)
}
\keyword{ package }
\seealso{
\pkg{PresenceAbsence}, \pkg{ROCR}, \pkg{verification}, \pkg{dismo}, \pkg{fuzzySim}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

# plot the 1st model:
plotGLM(model = mod)

# calculate the area under the ROC curve for the 1st model:
AUC(model = mod)

# calculate some more threshold-based measures for this model:
threshMeasures(model = mod, thresh = "preval")

# calculate optimal thresholds based on several criteria:
optiThresh(model = mod)

# calculate the optimal threshold balancing two evaluation measures:
optiPair(model = mod, measures = c("Omission", "Commission"))

# calculate the explained deviance, Hosmer-Lemeshow goodness-of-fit,
# Miller's calibration stats, and (pseudo) R-squared values for the 1st model:

Dsquared(model = mod)

HLfit(model = mod, bin.method = "quantiles")

MillerCalib(model = mod)

RsqGLM(model = mod)
}
