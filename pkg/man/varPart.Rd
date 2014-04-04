\name{varPart}
\alias{varPart}
\title{
Variation partitioning
}
\description{
This function performs variation partitioning (Borcard et al. 1992) among two factors (e.g. Ribas et al. 2006) or three factors (e.g. Real et al. 2003) for either multiple linear regression models (LM) or generalized linear models (GLM).
}
\usage{
varPart(A, B, C = NULL, AB, BC = NULL, AC = NULL, ABC = NULL, model.type, A.name = "A", B.name = "B", C.name = "C", plot = TRUE, plot.digits = 3, cex.names = 1.2, cex.values = 1)
}
\arguments{
  \item{A}{numeric value of the R-squared of the regression of the resonse variable on the variables related to factor 'A'}
  \item{B}{numeric value of the R-squared of the regression of the resonse variable on the variables related to factor 'B'}
  \item{C}{(optionally, if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables related to factor 'C'}
  \item{AB}{numeric value of the R-squared of the regression of the response on the variables of factors 'A' and 'B' simultaneously}
  \item{BC}{(if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables of factors 'B' and 'C' simultaneously
}
  \item{AC}{(if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables of factors 'A' and 'C' simultaneously}
  \item{ABC}{(if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables of factors 'A', 'B' and 'C' simultaneously}
  \item{model.type}{character value, either "LM" (linear model) or "GLM" (generalised linear model) depending on your model type.}
  \item{A.name}{optional character indicating the name of factor 'A'}
  \item{B.name}{optional character indicating the name of factor 'B'
}
  \item{C.name}{optional character indicating the name of factor 'C' (if there are 3 factors)}
  \item{plot}{logical, whether to plot the variation partitionig diagram.}
  \item{plot.digits}{integer value of the number of digits to which to round the values in the plot.}
  \item{cex.names}{numeric value indicating character expansion factor to define the size of the names of the factors displayed in the plot.}
  \item{cex.values}{numeric value indicating character expansion factor to define the size of the values displayed in the plot.}
}
\details{
If you have linear models, input data for \code{varPart} are the coefficients of determination (R-squared values) of the linear regressions of the target variable on all the variables in the model, on the variables related to each particular factor, and (when there are 3 factors) on the variables related to each pair of factors. The outputs are the amounts of variance explained exclusively by each factor, the amounts explained exclusively by the overlapping effects of each pair of factors, and the amount explained by the overlap of the 3 factors if this is the case (e.g. Real et al. 2003). The amount of variation not explained by the complete model is also provided.

If you have generalized linear models (GLMs) such as logistic regression (see \code{\link{glm}}) or the favourability function (see \code{\link{Fav}}), you have no R-squared values from these models; inputs are then the squared coefficients of (Spearman's) correlation between the model predictions given by each factor or pair of factors and the predictions of the complete model (e.g. Munoz & Real 2006), or the R-squared values of the corresponding logit (y) functions (Real et al. 2013), or an adjusted R-squared (De Araujo et al. 2013). Consequently, output values are not the total amounts of variance (of the target variable) explained by factors and overlaps, but rather their proportional contribution to the total variation explained by the model. The amount of unexplained variation is not available for GLMs.}
\value{
The output consists of a data frame indicating the proportion of variance accounted for by each of the factors, and (if \code{plot = TRUE} a Venn diagram of the contributions of each factor.)
}
\references{
Borcard D, Legendre P, Drapeau P (1992) Partialling out the spatial component of ecological variation. Ecology 73: 1045-1055

De Araujo, C.B., Marcondes-Machado, L.O. & Costa, G.C. (2013) The importance of biotic interactions in species distribution models: a test of the Eltonian noise hypothesis using parrots. Journal of Biogeography, early view (DOI: 10.1111/jbi.12234)

Munoz, A.-R. & Real, R. (2006) Assessing the potential range expansion of the exotic monk parakeet in Spain. Diversity and Distributions 12: 656-665.

Real, R., Barbosa, A.M., Porras, D., Kin, M.S., Marquez, A.L., Guerrero, J.C., Palomo, L.J., Justo, E.R. & Vargas, J.M. (2003) Relative importance of environment, human activity and spatial situation in determining the distribution of terrestrial mammal diversity in Argentina. Journal of Biogeography 30: 939-947.

Real R., Romero D., Olivero J., Estrada A. & Marquez A.L. (2013) Estimating how inflated or obscured effects of climate affect forecasted species distribution. PLoS ONE 8: e53646. doi:10.1371/journal.pone.0053646

Ribas, A., Barbosa, A.M., Casanova, J.C., Real, R., Feliu, C. & Vargas, J.M. (2006) Geographical patterns of the species richness of helminth parasites of moles (Talpa spp.) in Spain: separating the effect of sampling effort from those of other conditioning factors. Vie et Milieu 56: 1-8.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
}
\examples{
varPart(A = 0.857537, B = 0.078095, C = 0.499474, AB = 0.89933, BC = 0.492119, AC = 0.981672, model.type = "GLM", A.name = "Climatic", B.name = "Human", C.name = "Topographic", plot.digits = 3, cex.names = 1.2, cex.values = 1)
}
\keyword{ regression }
