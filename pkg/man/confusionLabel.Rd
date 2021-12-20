\name{confusionLabel}
\alias{confusionLabel}
\title{
Label predictions according to their confusion matrix category
}
\description{
This function labels the predictions of a binary-response model according to their confusion matrix categories, i.e., it classifies each prediction into a false positive, false negative, true positive or true negative, given a user-defined threshold value.
}
\usage{
confusionLabel(model = NULL, obs = NULL, pred = NULL, thresh, verbosity = 2)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a vector of observed presences (1) and absences (0) of a binary response variable. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. This argument is ignored if 'model' is provided.}
  \item{thresh}{numeric value of the threshold to separate predicted presences from predicted absences; can be "preval", to use the prevalence of 'obs' (or of the response variable in 'model') as the threshold, or any real number between 0 and 1. See Details in \code{\link{threshMeasures}} for an informed choice.}
    \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
}
\value{
This function returns a character vector of the same length as 'obs' and 'pred', or of the same number of rows as the data in 'model', containing the confusion matrix label for each value.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

confusionLabel(model = mod, thresh = 0.5)


# you can also use confusionLabel with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values

confusionLabel(obs = presabs, pred = prediction, thresh = 0.5)
}
