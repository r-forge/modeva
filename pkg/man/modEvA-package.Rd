\name{modEvA-package}
\alias{modEvA-package}
\alias{modEvA}
\docType{package}
\title{Model Evaluation and Analysis}
\description{
The modEvA package can build and analyse species distribution models and evaluate their performance. Includes functions for performing variation partitioning, assessing the false discovery rate, analysing multicollinearity among variables, calculating several measures of model discrimination and calibration, optimizing prediction thresholds based on a number of criteria, performing multivariate environmental similarity surface (MESS) analysis, and displaying various analytical plots. Includes also a sample data set with species occurrences and predictor variables.
}
\details{
\tabular{ll}{
Package: \tab modEvA\cr
Type: \tab Package\cr
Version: \tab 0.7.2\cr
Date: \tab 2014-11-04\cr
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
\pkg{PresenceAbsence}, \pkg{ROCR}, \pkg{verification}, \pkg{dismo}
}
\examples{
# load sample data and see the column names:
data(rotif.env)
names(rotif.env)

# analyse multicollinearity among predictor variables:
multicol(rotif.env[ , 5:17])

# build models for presence-absence of rotifer species:
mods <- multGLM(data = rotif.env, sp.cols = 18:47, var.cols = 5:17, 
step = FALSE, trim = TRUE)

# analyse the resulting predictions table:
head(mods$predictions)

# plot the 1st model:
plotGLM(model = mods$models[[1]])

# calculate the area under the ROC curve for the 1st model:
AUC(model = mods$models[[1]])

# calculate some more threshold-based measures for this model:
threshMeasures(model = mods$models[[1]], thresh = "preval")

# calculate optimal thresholds based on several criteria:
optiThresh(model = mods$models[[1]])

# calculate the optimal threshold balancing two evaluation measures:
optiPair(model = mods$models[[1]], measures = c("Omission", "Commission"))

# calculate the explained deviance, Hosmer-Lemeshow goodness-of-fit, 
# Miller's calibration stats, and (pseudo) R-squared values for the 1st model:

Dsquared(model = mods$models[[1]])

HLfit(model = mods$models[[1]])

MillerCalib(model = mods$models[[1]])

RsqGLM(model = mods$models[[1]])
}
