multicol <- function(data, var.cols = 1:ncol(data)) {
  result <- matrix(NA, nrow = length(var.cols), ncol = 3)
  rownames(result) <- colnames(data[ , var.cols])
  colnames(result) <- c("Rsquared", "Tolerance", "VIF")
  for (v in var.cols) {
    v.name <- colnames(data)[v]
    other.v.names <- colnames(data)[var.cols[-v]]
    mod.formula <- as.formula(paste(v.name, "~", paste(other.v.names, collapse = "+")))
    mod <- lm(mod.formula, data = data)
    R2 <- summary(mod) $ r.squared
    result[v, "Rsquared"] <- R2
    result[v, "Tolerance"] <- 1 - R2
    result[v, "VIF"] <- 1 / (1 - R2)
  }; rm(v)
  return(result)
}
