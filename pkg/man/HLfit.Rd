\name{HLfit}
\alias{HLfit}
\title{
Hosmer-Lemeshow goodness of fit
}
\description{
This function calculates a model's calibration performance (reliability) with the Hosmer & Lemeshow goodness-of-fit statistic, which compares predicted probability to observed occurrence frequency at each portion of the probability range.
}
\usage{
HLfit(model = NULL, obs = NULL, pred = NULL, bin.method, n.bins = 10,
fixed.bin.size = FALSE, min.bin.size = 15, min.prob.interval = 0.1, simplif = FALSE,
alpha = 0.05, plot = TRUE, plot.values = TRUE, plot.bin.size = TRUE,
xlab = "Predicted probability", ylab = "Observed prevalence", ...)
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another binary response variable. Not necessary (and ignored) if \code{model} is provided.}
  \item{pred}{a vector with the corresponding predicted probabilities as given e.g. by logistic regression. Not necessary (and ignored) if \code{model} is provided.}
  \item{bin.method}{the method for grouping the records into bins within which to compare predicted probability to observed prevalence; type modEvAmethods("getBins") for available options. See Details for more info.}
  \item{n.bins}{the number of bins to use if \code{bin.method = n.bins}.}
  \item{fixed.bin.size}{logical, whether to force bins to have the same size whenever possible.}
  \item{min.bin.size}{the minimum number of records in each bin (minimum for significant comparisons is at least 15 according to Jovani & Tella 2006).}
  \item{min.prob.interval}{the minimum interval (range) of probability values within each bin.}
  \item{simplif}{logical, wheter to perform a faster version returning only the basic statistics.}
  \item{alpha}{alpha value for confidence intervals if \code{plot = TRUE}.}
  \item{plot}{logical, whether to produce a plot of the results.}
  \item{plot.values}{logical, whether to report measure values in the plot.}
  \item{plot.bin.size}{logical, whether to report bin sizes in the plot.}
  \item{xlab}{label for the x axis.}
  \item{ylab}{label for the y axis.
}
  \item{\dots}{further arguments to pass to the \code{\link{plot}} function.}
}
\details{
Most of the commonly used measures for evaluating model performance focus on discrimination capacity, i.e., how well the model is capable of distinguishing presences from absences (after the model's continuous predictions of presence probability or alike are converted to binary predictions of presence or absence). However, there is another important facet of model evaluation: calibration or reliability, i.e., the relationship between predicted probability and observed prevalence (Pearce & Ferrier 2000; Jimenez-Valverde et al. 2013). The \code{HLfit} function measures model reliability with the Hosmer & Lemeshow goodness-of-fit statistic (Hosmer & Lemeshow 1980).

Note that this statistic has some limitations and caveats (see e.g. http://www.statisticalhorizons.com/hosmer-lemeshow), mainly due to the need to group the values into bins within which to compare probability and prevalence, and the influence of the binning method on the result. The \code{HLfit} function can use several binning methods, which are implemented in the \code{\link{getBins}} function and can be accessed by typing \code{modEvAmethods("getBins")}. You should therefore evaluate your model using different binning methods, to see if the results change significantly.
}
\value{
\code{HLfit} returns a list with the following components:
\item{bins.table}{a data frame of the obtained bins and the values resulting from the hosmer-Lemeshow goodness-of-fit analysis.}
\item{chi.sq}{the value of the Chi-squared test.}
\item{DF}{the number of degrees of freedom.}
\item{p.value}{the p-value of the Hosmer-Lemeshow test. Note that this is one of those tests for which higher p-values are better.}
\item{RMSE}{the root mean squared error.}
}
\references{
Hosmer D.W. & Lemeshow S. (1980) A goodness-of-fit test for the multiple logistic regression model. Communications in Statistics, A10: 1043-1069

Jimenez-Valverde A., Acevedo P., Barbosa A.M., Lobo J.M. & Real R. (2013) Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography, 22: 508-516

Jovani R. & Tella J.L. (2006) Parasite prevalence and sample size: misconceptions and solutions. Trends in Parasitology 22: 214-218

Pearce J. & Ferrier S. (2000) Evaluating the Predictive Performance of Habitat Models Developed using Logistic Regression. Ecological Modeling, 133: 225-245
}
\author{
A. Marcia Barbosa
}
\note{
The 4 lines of code from "observed" to "p.value" were adapted from the 'hosmerlem' function available at http://www.stat.sc.edu/~hitchcock/diseaseoutbreakRexample704.txt. The plotting code was loosely based on the \code{calibration.plot} function in package \pkg{PresenceAbsence}. \code{HLfit} still needs some code simplification, and may fail for some datasets and binning methods. Fixes are being applied. Feedback is welcome.
}
\seealso{
\code{\link{getBins}}, \code{\link{threshMeasures}}, \code{\link{AUC}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

HLfit(model = mod, bin.method = "n.bins", min.prob.interval = 0.1)

HLfit(obs = mod$y, pred = mod$fitted.values, bin.method = "quantiles", n.bins = 10)
}
\keyword{ }
