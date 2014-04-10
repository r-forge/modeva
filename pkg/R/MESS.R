MESS <- function(V, P){   
  # Alberto Jimenez Valverde, version 1.0, 20 March 2013
  
  if (NA %in% V | NA %in% P) stop("NA values are not allowed.")
  
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
    SIM <- c()
    for (j in 1:n.proj){
      VV <- V[, i]
      VV[VV < P[j, i]] <- 1
      VV[VV >= P[j, i]] <- 0
      Fj <- sum(VV) * 100 / length(VV)
      if(Fj == 0) SIM[j] <- (P[j, i] - min.Vi) / (max.Vi - min.Vi) * 100
      else if(Fj > 0 && Fj <= 50) SIM[j] <- 2 * Fj
      else if(Fj > 50 && Fj < 100) SIM[j] <- 2 * (100 - Fj)
      else if(Fj == 100) SIM[j] <- (max.Vi - P[j, i]) / (max.Vi - min.Vi) * 100
    }
    results[, i] <- SIM
  }
  
  for (t in 1:n.proj){
    results[t, i+1] <- min(results[t, 1:i])
  }
  return(results)
}