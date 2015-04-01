varPart <-
function(A, B, C = NULL, AB, BC = NULL, AC = NULL, ABC = NULL,
         model.type, A.name = "A", B.name = "B", C.name = "C",
         plot = TRUE, plot.digits = 3, cex.names = 1.2,
         cex.values = 1) {

  twofactors <- is.null(C)

  if (model.type == "LM") {
    totalexpl <- ifelse(twofactors, AB, ABC)
    unexpl <- 1 - totalexpl
  }  # end if LM

  else if (model.type == "GLM") {
    totalexpl <- 1
    unexpl <- NA
  }  # end if GLM

  else stop("'model.type' must be either 'LM' or 'GLM'")

  if(twofactors) {
    Apure <- totalexpl - B
    Bpure <- totalexpl - A
    ABoverlap <- totalexpl - Apure - Bpure
    output.names <- c(paste("Pure", A.name), paste("Pure", B.name),
                      paste("Pure ", A.name, "-", B.name, " overlap", sep = ""),
                      "Unexplained variation")
    results <- data.frame(c(Apure, Bpure, ABoverlap, unexpl),
                          row.names = output.names)
  }  # end if 2 factors
  else {
    Apure <- totalexpl - BC
    Bpure <- totalexpl - AC
    Cpure <- totalexpl - AB
    ABoverlap <- totalexpl - Apure - Bpure - C
    BCoverlap <- totalexpl - Bpure - Cpure - A
    ACoverlap <- totalexpl - Apure - Cpure - B
    ABCoverlap <- totalexpl - Apure - Bpure - Cpure - ABoverlap - BCoverlap - ACoverlap
    output.names <- c(paste("Pure", A.name),
                      paste("Pure", B.name),
                      paste("Pure", C.name),
                      paste("Pure ", A.name, "-", B.name, " overlap", sep = ""),
                      paste("Pure ", B.name, "-", C.name, " overlap", sep = ""),
                      paste("Pure ", A.name,"-", C.name," overlap", sep = ""),
                      paste(A.name,"-",B.name,"-",C.name," overlap", sep = ""),
                      "Unexplained variation")
    results <- data.frame(c(Apure, Bpure, Cpure, ABoverlap, BCoverlap,
                            ACoverlap, ABCoverlap, unexpl),
                          row.names = output.names)
  }  # end else

  colnames(results) <- "Proportion"
  #n <- nrow(results)
  #if(model.type == "GLM")  results <- results[1:(n-1),]  # deletes "unexplained" line (data unavailable for GLM)

  if(plot) {  # adapted from Daniel's http://stackoverflow.com/questions/1428946/venn-diagrams-with-r

    circle <- function(x, y, r) {
      ang <- seq(0, 2*pi, length = 100)
      xx <- x + r * cos(ang)
      yy <- y + r * sin(ang)
      polygon(xx, yy)
    }  # end circle funtion (by Daniel)

    Apure <- round(Apure, plot.digits)  # shorten values for plotting
    Bpure <- round(Bpure, plot.digits)
    ABoverlap <- round(ABoverlap, plot.digits)
    if(!twofactors) {
      Cpure <- round(Cpure, plot.digits)
      BCoverlap <- round(BCoverlap, plot.digits)
      ACoverlap <- round(ACoverlap, plot.digits)
      ABCoverlap <- round(ABCoverlap, plot.digits)
    }

    if(twofactors) {
      plot(0, 0, ylim = c(-1, 10), xlim = c(-1, 7), type = "n", axes = FALSE,
           ylab = "", xlab = "")
      circle(3,3,3)
      circle(3,6,3)
      text(x = c(3, 3), y = c(9.5, -0.5), labels = c(A.name, B.name),
           cex = cex.names)
      text(x = c(3, 3, 3), y = c(7, 4.75, 2), c(Apure, ABoverlap, Bpure),
           cex = cex.values)
    }  # end if 2 factors
    else {
      plot(0, 0, ylim = c(-1, 10), xlim = c(-1, 10), type = "n", axes = FALSE,
           ylab = "", xlab = "")
      circle(3,6,3)
      circle(6,6,3)
      circle(4.5,3,3)
      text(x = c(2.5, 6.5, 4.5), y = c(9.5, 9.5, -0.5),
           labels = c(A.name, B.name, C.name), cex = cex.names)
      text(x = c(1.8, 7.2, 4.5, 4.5, 2.8, 6.2, 4.5), y = c(6.6, 6.6, 2, 7, 4, 4, 5),
           labels = c(Apure, Bpure, Cpure, ABoverlap, ACoverlap, BCoverlap,
                      ABCoverlap), cex = cex.values)
    } # end else
  }  # end if plot
  return(results)
}
