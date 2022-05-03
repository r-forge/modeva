modEvAmethods <- function(fun) {
  # version 1.4 (11 Apr 2022)
  
  if (fun %in% c("threshMeasures", "multModEv")) {
    thresh.measures <- c("CCR", "Misclass", "Sensitivity", "Specificity", "Omission", "Commission", "Precision", "Recall", "PPP", "NPP", "UPR", "OPR", "PPI", "PAI", "kappa", "TSS", "NMI", "F1score", "OddsRatio")

    if (fun == "threshMeasures") return(thresh.measures)
    
    else if (fun == "multModEv") {
      bin.measures <- c("HL", "HL.p", "RMSE")  # "ABCc", "rABCc", "unityRsq"
      return (c("Prevalence", "AUC", "MeanPrecision", "AUCPR", thresh.measures, bin.measures, "Miller.int", "Miller.slope"))  # "Evenness", "ABCc", "rABCc", "unityRsq", "Miller.p"
    }  # end if multModEv
  }  # end if threshMeasures | multModEv
  
  else if (fun == "getBins") return(c("round.prob", "prob.bins", "size.bins", "n.bins", "quantiles"))
  
  else if (fun == "optiThresh") return(c("each", "preval", "0.5", "maxKappa", "minSensSpecDiff", "maxSensSpecSum", "maxTSS"))
  
  else if (fun == "getThreshold") return(sort(c("preval", "trainPrev", "meanPred", "midPoint", "maxKappa", "maxCCR", "maxOPS", "maxOA", "maxF", "maxSSS", "maxTSS", "minDSS", "minDPR", "maxSPR", "minD01", "minD11", "equalPrev", "MTP")))
  
  else stop ("Invalid 'fun' argument. Check (case-sensitive) spelling.")
}
