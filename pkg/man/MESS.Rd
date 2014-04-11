\name{MESS}
\alias{MESS}
\title{
Multivariate Environmental Similarity Surfaces based on a data frame
}
\description{
This function performs the MESS analysis of Elith et al. (2010) to determine 
the extent of the environmental differences between model training and model 
projection (extrapolation) data. It is applicable to variables in a matrix 
or data frame.
}
\usage{
MESS(V, P)
}
\arguments{
  \item{V}{a matrix or data frame containing the variables (one in each column) 
in the reference (training) dataset.}
  \item{P}{a matrix or data frame containing the same variables in the area to 
  which the model(s) will be projected. Variables (columns) must be in the same 
  order as in \code{V}, and \code{colnames(P)} must exist.}
}
\details{
When model predictions are projected into regions, times or spatial resolutions 
not analysed in the training data, it may be important to measure the similarity 
between the new environments and those in the training sample (Elith et al. 
2010), as models are not so reliable when predicting outside their domain 
(Barbosa et al. 2009). The Multivariate Environmental Similarity Surfaces (MESS) 
analysis measures the similarity in the analysed variables between any given 
locality in the projection dataset and the localities in the reference 
(training) dataset (Elith et al. 2010).

MESS analysis is implemented in the MAXENT software (Phillips et al. 
2006) and in the \code{\pkg{dismo}} R package, but there it requires input 
variables in raster format. This implies not only the use of complex spatial 
data structures, but also that the units of analysis are rectangular pixels, 
whereas we often need to model distribution data recorded on irregular 
units (e.g. provinces, river basins), or on equal-area units (e.g. UTM cells, 
equal-area hexagons), whereas pixels vary in area between the equator and the 
poles. The MESS function computes this analysis for variables in a data frame, 
where localities (in rows) may be of various sizes and shapes.
}
\value{
A matrix with the same input column names as \code{P}, plus a column named 
\code{TOTAL}, quantifying the similarity between each point in the projection 
dataset and those in the reference dataset. Negative values indicate localities 
that are environmentally dissimilar from the reference region.
}
\references{
Barbosa A.M., Real R. & Vargas J.M. (2009) Transferability of environmental favourability models in geographic space: the case of the Iberian desman (Galemys pyrenaicus) in Portugal and Spain. Ecological Modelling 220: 747-754

Elith J., Kearney M. & Phillips S. (2010) The art of modelling range-shifting 
species. Methods in Ecology and Evolution 1: 330-342

Phillips S.J., Anderson R.P. & Schapire R.E. (2006) Maximum entropy modeling 
of species geographic distributions. Ecological Modelling 190: 231-259
}
\author{
Alberto Jimenez-Valverde
}
\note{
}
\seealso{
\code{\link{mess}} in package \code{\pkg{dismo}}; \code{\link{OA}}
}
\examples{
# load the sample data:

data(rotif.env)


# add a column specifying the hemisphere:

unique(rotif.env$CONTINENT)

rotif.env$HEMISPHERE <- "Eastern"

rotif.env$HEMISPHERE[rotif.env$CONTINENT \%in\% 
c("NORTHERN_AMERICA", "SOUTHERN_AMERICA")] <- "Western"

head(rotif.env)


# perform a MESS analysis 
# suppose you'll extrapolate models from the Western hemisphere (Americas)
# to the Eastern hemisphere (rest of the world):

names(rotif.env)  # variables are in columns 5:17

mess <- MESS(V = subset(rotif.env, HEMISPHERE == "Western", select = 5:17), 
P = subset(rotif.env, HEMISPHERE == "Eastern", select = 5:17))

head(mess)
range(mess[ , "TOTAL"])

}
\keyword{ models }
\keyword{ extrapolation }
