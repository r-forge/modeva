OA <-
function(response, predictors) {
  # version 1.1 (20 May 2013)
  # response: a vector or column with your response presence-absence variable
  # predictors: the corresponding variables whose values you want to circumscribe to positive (presence) values of the response; must be in the same order and of the same length as 'response'
  predictors.presence <- predictors[response > 0, ]
  nvar <- ncol(predictors)
  var.presmin <- var.presmax <- vector("numeric", nvar)
  predictors.overlap <- matrix(data = NA, nrow = nrow(predictors), ncol = nvar)
  for (v in 1:nvar) {
    var.presmin[v] <- min(predictors.presence[ ,v])
    var.presmax[v] <- max(predictors.presence[ ,v])
    predictors.overlap[ ,v] <- ifelse((predictors[ ,v] >= var.presmin[v] & predictors[ ,v] <= var.presmax[v]), 1, 0)
  }  # end for v
  overlap <- as.integer(rowSums(predictors.overlap) == nvar)
  return(overlap)
}
