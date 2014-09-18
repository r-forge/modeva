\name{multModEv}
\alias{multModEv}
\title{
Multiple model evaluation
}
\description{
If you have a list of models created e.g. with the \code{\link{multGLM}} function, or a data frame with presence-absence data and the corresponding predicted values for a set of species, you can use the \code{multModEv} function to get a set of evaluation measures for all models simultaneously. Note that all models must have the same sample size.
}
\usage{
multModEv(obs.data = NULL, pred.data = NULL, models = NULL, Favourability = FALSE, 
measures = modEvAmethods("multModEv"), thresh = "preval", standardize = FALSE, 
bin.method = "quantiles", quiet = TRUE)
}
\arguments{
  \item{obs.data}{a data frame with observed binary data.}
  \item{pred.data}{a data frame with the corresponding predicted probability values, with both rows and columns in the same order as in \code{obs.data}.}
  \item{models}{instead of (and overriding) \code{obs.data} and \code{pred.data}, a list of model objects of class "glm" applied to the same data set.}
  \item{Favourability}{logical, whether to convert predicted probability to \code{\link{Fav}}ourability before calculating fixed-threshold measures; used only if model objects are provided (rather than obs.data + pred.data).}
  \item{measures}{character vector of the evaluation measures to calculate. The default is all implemented measures, which you can check by typing \code{modEvAmethods("multModEv")}.}
  \item{thresh}{the threshold value to use for calculating threshold-based measures (the ones in \code{modEvAmethods("threshMeasures")}). Can be "preval" (the default, for species prevalence) or any number between 0 and 1.}
  \item{standardize}{logical, whether to standardize measures that vary between -1 and 1 to the 0-1 scale (\code{\link{standard01}}).}
  \item{bin.method}{the method with which to divide the data into groups or bins (for calibration or reliability measures such as \code{\link{HLfit}}); type modEvAmethods("getBins") for available options).}
  \item{quiet}{logical, whether to be wuiet or display messages.}
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
data(rotif.env)

names(rotif.env)


# make models for 3 of the species in rotif.env:

mods <- multGLM(rotif.env, sp.cols = 18:20, var.cols = 5:17, id.col = 1, step = TRUE, 
FDR = TRUE, trim = TRUE)

names(mods$predictions)


# calculate some evaluation measures for these models:

eval <- multModEv(obs.data = rotif.env[ , 18:20], 
pred.data = mods$predictions[ , c("Abrigh_P", "Afissa_P", "Apriod_P")], 
thresh = "preval")

head(eval)

eval2 <- multModEv(models = mods$models, thresh = 0.7, 
measures = c("AUC", "CCR", "Sensitivity", "TSS"))

head(eval2)

}
\keyword{ models }
\keyword{ evaluation }
\keyword{ performance }

