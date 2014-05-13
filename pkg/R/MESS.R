MESS <- function(V, P){   
  # version 1.1, 7 May 2014
  
  if (NA %in% as.matrix(V) | NA %in% as.matrix(P)) stop("NA values are not allowed.")
  
  index.V <- 1:nrow(V)
  index.P <- 1:nrow(P)
  
  n.vars <- ncol(V)
  n.varP <- ncol(P)
  
  if(n.vars != n.varP) stop("The number of variables in V and P does not match.")
  
  n.proj <- nrow(P)
  
  results <- matrix(nrow = n.proj, ncol = n.vars + 1)
  colnames(results) <- c(colnames(P), "TOTAL")
  #results <- as.data.frame(results)
  
  for (i in 1:n.vars){
    min.Vi <- min(V[, i])
    max.Vi <- max(V[, i])
    SIM <- vector("numeric", n.proj)
    for (j in 1:n.proj){
      VV <- V[, i]
      VV[VV < P[j, i]] <- 1
      VV[VV >= P[j, i]] <- 0
      Fj <- sum(VV) * 100 / length(VV)
      if (Fj == 0)  SIM[j] <- (P[j, i] - min.Vi) / (max.Vi - min.Vi) * 100
      else if (Fj > 0 && Fj <= 50)  SIM[j] <- 2 * Fj
      else if (Fj > 50 && Fj < 100)  SIM[j] <- 2 * (100 - Fj)
      else if (Fj == 100)  SIM[j] <- (max.Vi - P[j, i]) / (max.Vi - min.Vi) * 100
    }
    results[, i] <- SIM
  }
  
  for (t in 1:n.proj){
    results[t, i+1] <- min(results[t, 1:i])
  }
  return(results)
}