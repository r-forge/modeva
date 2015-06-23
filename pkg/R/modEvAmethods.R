modEvAmethods <- function(fun) {
  if (fun %in% c("threshMeasures", "multModEv")) {
    thresh.measures <- c("CCR", "Misclass", "Sensitivity", "Specificity",
                         "Omission", "Commission", "PPP", "NPP", "UPR", "OPR",
                         "PPI", "PAI", "kappa", "TSS", "NMI", "OddsRatio")
    if (fun == "threshMeasures") return(thresh.measures)
    else if (fun == "multModEv") return (c("Prevalence", "AUC", thresh.measures, "HL", "HLp", "MillerInt", "MillerSlope", "MillerTest"))  # "Evenness", "ABCc", "rABCc", "unityRsq"
  }
  else if (fun == "getBins") return(c("round.prob", "prob.bins", "size.bins",
                                  "n.bins", "quantiles"))
  else if (fun == "optiThresh") return(c("each", "preval", "0.5", "maxKappa",
                                        "minSensSpecDiff", "maxSensSpecSum",
                                        "maxTSS"))
}
