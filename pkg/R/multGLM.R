multGLM <- function(data, sp.cols, var.cols, id.col = NULL, family = "binomial",
                    test.sample = 0, FDR = FALSE, step = TRUE, trace = 0, 
                    start = "null.model", direction = "both", y = TRUE, P = TRUE,
                    Favourability = TRUE, group.preds = TRUE, trim = TRUE, ...) {
  
  # version 3.2 (13 May 2014)
  # data: data frame with your binary species data and variables
  # sp.cols: index numbers of the columns containing the species data to be modelled; should contain only binary data (0 or 1)
  # var.cols: index numbers of the columns containing the predictor variables to be used
  # id.col: (optional) index number of column containing the row identifiers (will be included in predictions table if defined)
  # family: argument to be passed to the glm function; only 'binomial' is implemented so far
  # test.sample: subset of data to set aside for model testing. Can be a value between 0 and 1 for a proportion of the data to choose randomly (e.g. 0.2 for 20%), or an  integer number for a particular number of cases, or a vector of integers for the concrete cases to set aside, or "Huberty" for his rule of thumb based on the number of variables (see Fielding & Bell 1997)
  # FDR: logical, whether or not to do a preliminary exclusion of variables based on their bivariate relationship with the response and the false discovery rate
  # step: logical, whether or not use the step function to perform a first stepwise variable selection based on AIC
  # trace: arg to be pased to step (see ?step for details)
  # start: (used if step = TRUE) whether to start with the 'null.model' (so variable selection starts forward) or with the 'full.model' (so selection starts backward)
  # direction: (used if step = TRUE) argument to be passed to the 'step' function specifying the direction of variable selection ('forward', 'backward' or 'both')
  # y: logical, whether or not to include in the output the response in the scale of the predictor variables (logit)
  # P: logical, whether or not to include in the output the response in the probability scale (response)
  # Favourability: logical, whether or not to apply the favourability function (Real et al. 2006) and include its results in the output; requires the 'Fav' function
  # group.preds: logical, whether or not to group together predictions of similar type (y, P or F) in the output predictions table (e.g. if FALSE: sp1_y, sp1_P, sp1_F, sp2_y, sp2_P, sp2_F; if TRUE: sp1_y, , sp2_y, sp1_P, sp2_P, sp1_F, sp2_F)
  # trim: logical, whether or not to trim non-significant variables off the models (requires the 'modelTrim' function); can be used whether or not step is TRUE; works as a backward variable elimination procedure based on significance
  # ...: additional arguments to be passed to the 'modelTrim' function
  
  start.time <- proc.time()
  input.ncol <- ncol(data)
  
  stopifnot (
    as.vector(na.omit(as.matrix(data[ , sp.cols]))) %in% c(0,1),
    sp.cols %in% (1 : input.ncol),
    var.cols %in% (1 : input.ncol),
    is.null(id.col) | id.col %in% (1 : input.ncol),
    family == "binomial",
    test.sample >= 0 | test.sample == "Huberty",
    length(test.sample) == 1  | (is.integer(test.sample) & test.sample > 0),
    length(test.sample) < nrow(data),
    is.logical(FDR),
    is.logical(step),
    start %in% c("null.model", "full.model"),
    direction %in% c("both", "backward", "forward"),
    is.logical(y),
    is.logical(P),
    is.logical(Favourability),
    is.logical(group.preds),
    is.logical(trim),
    !Favourability | exists("Fav"),
    !trim | exists("modelTrim")
  )
  
  data$sample <- "train"
  n <- nrow(data)
  data.row <- 1:n
  
  if (length(test.sample) == 1) {
    if (test.sample == "Huberty") {
      if (!FDR & !step & !trim) {
        test.sample <- percentTestData(length(var.cols)) / 100
        n.test <- round(n * test.sample)
        message(
"Following Huberty's rule, ", test.sample * 100, "% of observations 
(", n.test, " out of ", n, ") set aside for model testing; ", 
n - n.test, " observations used for model training.")
      } else stop ("
Sorry, Huberty's rule cannot be used with FDR, step or trim, as these make 
the number of variables differ among models. Set these 3 parameters to FALSE, 
or use a different 'test.sample' option.")
    }  # end if Huberty
    else if (test.sample == 0) {
      message("
All ", n, " observations used for model training; 
none reserved for model testing.")
      n.test <- 0
    } else if (test.sample < 1) {
      n.test <- round(n * test.sample)
      message(
test.sample * 100, "% of observations (", n.test, " out of ", n, ") set aside for model testing; ", 
n - n.test, " observations used for model training.")
    } else if (test.sample >= 1) {
      n.test <- test.sample
      message(
        n.test, " (out of ", n, ") observations set aside for model testing; ", 
        n - n.test, " observations used for model training.")
    }
    test.sample <- sample(data.row, size = n.test, replace = FALSE)
    } else if (length(test.sample) > 1) {
      n.test <- length(test.sample)
      message(
        n.test, " (out of ", n, ") observations set aside for model testing; ", 
        n - n.test, " observations used for model training.")
    }
  
  data$sample[data.row %in% test.sample] <- "test"
  train.data <- data[data$sample == "train", ]
  
  if (Favourability) {
    if(family != "binomial") {
      Favourability <- FALSE
      warning("Favourability is only applicable to binomial responses, 
              so it could not be calculated")
    }  # end if family != binomial (for when other families are implemented)
  }  # end if Fav
  
  keeP <- P  # keep P only if the user wants it
  if (Favourability)  P <- TRUE  # P is necessary to calculate Fav
  n.models <- length(sp.cols)
  models <- vector("list", n.models)
  model.count <- 0
  
  attach(train.data, warn.conflicts = FALSE)
  
  for (s in sp.cols) {
    model.count <- model.count + 1
    response <- colnames(train.data)[s]
    message("\nBuilding model ", model.count, " of ", n.models, 
            " (", response, ")...")
    
    if (FDR) {
      fdr <- FDR(data = train.data, sp.cols = s, var.cols = var.cols, model.type = "GLM")
      if (nrow(fdr$select) == 0) {
        warning(paste0(
"No variables passed the FDR test (so no model calculated) for\n"
, response, "; consider using FDR = FALSE?"))
        next
      }
      else if (nrow(fdr$select) < length(var.cols)) {
        cat("FDR-excluded variables:", 
            paste(row.names(fdr$exclude), collapse = ", "), "\n\n")
        cat("FDR-selected variables (starting with most significant):", 
            paste(row.names(fdr$select), collapse = ", "), "\n\n")
      }
      sel.var.cols <- which(colnames(train.data) %in% rownames(fdr$select))
    } else sel.var.cols <- var.cols
    
    model.formula <- as.formula(paste(response, "~", 
                    paste(colnames(train.data)[sel.var.cols], 
                    collapse = "+")))
    model.expr <- expression(glm(model.formula, family = binomial))
    
    if (step) {
      if (start == "full.model") {
        model <- step(eval(model.expr), direction = direction, trace = trace)          
      } else if (start == "null.model") {
        model.scope <- model.formula[-2]  # removes response from formula
        null.formula <- as.formula(paste(response, "~", 1))
        model <- step(glm(null.formula, family = binomial), 
                      direction = direction, scope = model.scope, trace = trace)
      } else stop("'start' must be either 'full.model' or 'null.model'")
    } else model <- eval(model.expr)
    
    if (trim)  model <- modelTrim(model, ...)
    
    if (step | trim) {
      cat("Multivariate variable selection performed with:")
      if (step) cat(" step")
      if (step & trim) cat(" +")
      if (trim) cat(" modelTrim")
      cat("\n\n")
      sel.var.names <- names(model$coefficients)[-1]
      cat(length(sel.var.names), " (out of ", length(var.cols), " input) variables included in the final model: ", 
          paste(sel.var.names, collapse = ", "), sep = "")
    }
    
    models[[model.count]] <- model
    names(models)[[model.count]] <- response
    
    if (y) {
      data[ , ncol(data) + 1] <- predict(model, data)
      colnames(data)[ncol(data)] <- paste(response, "y", sep = "_")
    }
    if (P) {
      data[ , ncol(data) + 1] <- predict(model, data, type = "response")
      colnames(data)[ncol(data)] <- paste(response, "P", sep = "_")
    }
    if (Favourability) {
      n1 <- sum(train.data[ , s] == 1)
      n0 <- sum(train.data[ , s] == 0)
      data[ , ncol(data) + 1] <- Fav(n1n0 = c(n1, n0), pred = data[ , ncol(data)])
      colnames(data)[ncol(data)] <- paste(response, "F", sep = "_")
      if (!keeP) data <- data[ , -(ncol(data) - 1)]
      } # end if Fav
  }  # end for s
  
  detach(train.data)
  models <- models[!sapply(models, is.null)]
  n.pred.types = sum(y, keeP, Favourability)  # sums logical values of function arguments
  
  if (n.pred.types == 0) {
    predictions <- data.frame()
  } else {
    predictions <- data[ , c(id.col, ((input.ncol + 1) : ncol(data)))]
    
    if (n.pred.types == 1 | length(sp.cols) == 1)  group.preds <- FALSE
    
    if (group.preds) {
      first.pred.col <- ifelse(is.null(id.col), 2, 3) # 1st new col is 'sample'
      pred1.cols <- seq(first.pred.col, ncol(predictions), by = n.pred.types)
      pred2.cols <- seq(first.pred.col + 1, ncol(predictions), by = n.pred.types)
      pred3.cols <- NULL
      if (n.pred.types == 3) {
        pred3.cols <- seq(first.pred.col + 2, ncol(predictions), 
                          by = n.pred.types)
      }  # end if pred.types > 2
      predictions <- data.frame(data[ , id.col], 
                                sample = data$sample,
                                predictions[ , pred1.cols], 
                                predictions[ , pred2.cols], 
                                predictions[ , pred3.cols])
    }  # end if groups.preds
    
    if (!is.null(id.col)) {
      colnames(predictions)[1] <- colnames(data)[id.col]
    }
    
  }  # end if pred.types 0 else
  
  #if (test.sample == 0) 
  #   predictions <- predictions[ , - match("sample", colnames(predictions))]

end.time <- proc.time()
  duration <- (end.time - start.time)[3]
  if (duration < 60) {
    units <- " second(s)."
  } else if (duration < 3600) {
    duration <- duration / 60
    units <- " minute(s)."
  } else {
    duration <- duration / 3600
    units <- " hour(s)."
  }
  
  message("Finished in ", round(duration), units)
  return(list(predictions = predictions, models = models))
  
  }  # end multGLM function
