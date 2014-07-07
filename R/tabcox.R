tabcox <- function(x, time, delta, latex = FALSE, xlabels = NULL, decimals = 2, p.decimals = c(2, 3), 
                   p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = TRUE, 
                   events = TRUE, coef = "n") {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
  }
  if (!is.numeric(p.decimals)) {
    stop("For p.decimals input, please enter numeric value or vector")
  }
  if (!is.numeric(p.cuts)) {  
    stop("For p.cuts input, please enter numeric value or vector")
  }
  if (!is.numeric(p.lowerbound)) {
    stop("For p.lowerbound input, please enter numeric value")
  }
  if (!is.logical(p.leading0)) {
    stop("For p.leading0 input, please enter TRUE or FALSE")
  }
  if (!is.logical(p.avoid1)) {
    stop("For p.avoid1 input, please enter TRUE or FALSE")
  }
  if (!is.logical(n)) {
    stop("For n input, please enter TRUE or FALSE")
  }
  if (!is.logical(events)) {
    stop("For events input, please enter TRUE or FALSE")
  }
  if (! coef %in% c("n", "x")) {
    stop("For coef input, please enter 'n' or 'x'")
  }
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # Set x to data frame if not already
  x <- as.data.frame(x)
  colx <- ncol(x)
  
  # Drop observations with missing values for one or more predictors
  locs <- complete.cases(x) & !is.na(time) & !is.na(delta)
  x <- as.data.frame(x[locs, ])
  time <- time[locs]
  delta <- delta[locs]
  
  # Get number of levels in each variable in x
  rows <- c()
  for (ii in 1:colx) {
    if (!is.factor(x[, ii]) | (is.factor(x[, ii]) & length(unique(x[, ii])) == 2)) {
      rows[ii] <- 1
    } else {
      rows[ii] <- length(unique(x[, ii]))+1
    }
  }
  
  # If xlabels not specified, create generic values
  if (is.null(xlabels)) {
    xlabels <- c()
    index <- 0
    for (ii in 1:colx) {
      if (rows[ii] == 1) {
        index <- index + 1
        xlabels[index] <- paste("Predictor ", ii, sep = "")
      } else {
        index <- index + 1
        xlabels[index] <- paste("Predictor ", ii, sep = "")
        index <- index + 1
        xlabels[index] <- "Level 1 (ref)"
        for (jj in 2:(rows[ii]-1)) {
          index <- index + 1
          xlabels[index] <- paste("Level ", jj, sep = "")
        }
      }
    }
  }
  
  # Add spaces in front of levels of factor variables for better appearance
  for (ii in 1:length(rows)) {
    if (ii == 1 & rows[ii] > 1) {
      xlabels[2:rows[ii]] <- paste("  ", xlabels[2:rows[ii]], sep = "")
      xlabels[2] <- paste(xlabels[2], " (ref)", sep = "")
    }
    if (ii > 1 & rows[ii] > 1) {
      xlabels[(sum(rows[1:(ii-1)])+2):sum(rows[1:ii])] <- paste("  ", xlabels[(sum(rows[1:(ii-1)])+2):sum(rows[1:ii])], sep = "")
      xlabels[(sum(rows[1:(ii-1)])+2)] <- paste(xlabels[(sum(rows[1:(ii-1)])+2)], " (ref)", sep = "")
    }
  }
  
  # Standardize variables if necessary
  if (coef == "x") {
    for (ii in 1:colx) {
      if (!is.factor(x[, ii]) & length(unique(x[, ii])) > 2) {
        x[, ii] <- (x[, ii]-mean(x[, ii]))/sd(x[, ii])
      }
    }
  }
  
  # Create survival object
  survobj <- Surv(time = time, event = delta)
  
  # Run Cox PH regression depending on number of x variables
  if (colx == 1) {fit <- summary(coxph(formula = survobj ~ x[, 1]))}
  if (colx == 2) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2]))}
  if (colx == 3) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3]))}
  if (colx == 4) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4]))}
  if (colx == 5) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5]))}
  if (colx == 6) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5] + x[, 6]))}
  if (colx == 7) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5] + x[, 6] + x[, 7]))}
  if (colx == 8) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5] + x[, 6] + x[, 7] + x[, 8]))}
  if (colx == 9) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5] + x[, 6] + x[, 7] + x[, 8] + x[, 9]))}
  if (colx == 10) {fit <- summary(coxph(formula = survobj ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5] + x[, 6] + x[, 7] + x[, 8] + x[, 9] + x[, 10]))}
  
  # Initialize table
  tbl <- matrix("", nrow = sum(rows), ncol = 7)
  tbl[1, 2] <- sum(locs)
  tbl[1, 3] <- sum(delta)
  
  # Enter values in table
  coef.index <- 0
  tbl.index <- 0
  for (ii in 1:colx) {
    if (rows[ii] == 1) {
      coef.index <- coef.index+1
      tbl.index <- tbl.index+1
      beta <- fit$coefficients[coef.index, 1]
      se <- fit$coefficients[coef.index, 3]
      hr <- exp(beta)
      p <- fit$coefficients[coef.index, 5]
      tbl[tbl.index, 4] <- paste(sprintf(spf, beta), " (", sprintf(spf, se), ")", sep = "")
      tbl[tbl.index, 5] <- sprintf(spf, hr)
      tbl[tbl.index, 6] <- paste("(", sprintf(spf, exp(beta-1.96*se)), ", ", sprintf(spf, exp(beta+1.96*se)), ")", sep = "")
      tbl[tbl.index, 7] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound,
                                   leading0 = p.leading0, avoid1 = p.avoid1)
    } else {
      tbl[(tbl.index+2), 4:7] <- "-"
      tbl.index <- tbl.index+2
      for (jj in 1:(rows[ii]-2)) {
        coef.index <- coef.index+1
        tbl.index <- tbl.index+1
        beta <- fit$coefficients[coef.index, 1]
        se <- fit$coefficients[coef.index, 3]
        hr <- exp(beta)
        p <- fit$coefficients[coef.index, 5]
        tbl[tbl.index, 4] <- paste(sprintf(spf, beta), " (", sprintf(spf, se), ")", sep = "")
        tbl[tbl.index, 5] <- sprintf(spf, hr)
        tbl[tbl.index, 6] <- paste("(", sprintf(spf, exp(beta-1.96*se)), ", ", sprintf(spf, exp(beta+1.96*se)), ")", sep = "")
        tbl[tbl.index, 7] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound,
                                     leading0 = p.leading0, avoid1 = p.avoid1)
      }
    }
  }
  
  # Add column names
  colnames(tbl) <- c("Variable", "N", "Events", "Beta (SE)", "HR", "95% CI for HR", "P")
  
  # Add variable names
  tbl[1:nrow(tbl)] <- xlabels
  
  # Drop particular columns if requested
  if (n == FALSE) {
    tbl <- tbl[, colnames(tbl) != "N", drop = FALSE]
  }
  if (events == FALSE) {
    tbl <- tbl[, colnames(tbl) != "Events", drop = FALSE]
  }
  
  # If latex is TRUE, do some re-formatting
  if (latex == TRUE) {
    plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
    if (length(plocs) > 0) {
      tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
    }
    spacelocs <- which(substr(tbl[, "Variable"], 1, 2) == "  ")
    if (length(spacelocs) > 0) {
      tbl[spacelocs, "Variable"] <- paste("\\hskip .3cm ", substring(tbl[spacelocs, "Variable"], 3), sep = "")
    }
  }
  
  # Return table
  return(tbl)
  
}