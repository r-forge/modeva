MESS <- function(V, P){   
  # version 1.4, 21 May 2014
  
  start.time <- Sys.time()
  index.V <- 1:nrow(V)
  index.P <- 1:nrow(P)
  n.vars <- ncol(V)
  n.varP <- ncol(P)
  if(n.vars != n.varP) stop("The number of variables in V and P does not match.")
  nrow.P <- nrow(P)
  results <- matrix(nrow = nrow.P, ncol = n.vars, dimnames = list(NULL, colnames(P)))
  
  for (i in 1:n.vars){
    message("Comparing variable ", i, " of ", n.vars, "...")
    min.Vi <- min(V[, i], na.rm = TRUE)
    max.Vi <- max(V[, i], na.rm = TRUE)
    SIM <- vector("numeric", nrow.P)
    for (j in 1:nrow.P){
      VV <- V[, i]
      VV[VV < P[j, i]] <- 1
      VV[VV >= P[j, i]] <- 0
      Fj <- sum(VV, na.rm = TRUE) * 100 / length(VV)
      if (Fj == 0)  SIM[j] <- (P[j, i] - min.Vi) / (max.Vi - min.Vi) * 100
      else if (Fj > 0 && Fj <= 50)  SIM[j] <- 2 * Fj
      else if (Fj > 50 && Fj < 100)  SIM[j] <- 2 * (100 - Fj)
      else if (Fj == 100)  SIM[j] <- (max.Vi - P[j, i]) / (max.Vi - min.Vi) * 100
    }
    results[, i] <- SIM
  }
  
  message("Calculating MESS and MoD...")
  results <- data.frame(results)
  results$TOTAL <- apply(results[ , 1:n.vars], 1, min)
  results$MoD <- as.factor(colnames(results)[apply(results[ , 1:n.vars], 1, which.min)])
  message("Finished!")
  timer(start.time)
  return(results)
}