tabmeans <- function(x, y, latex = FALSE, xlevels = NULL, yname = "Y variable", decimals = 1,
                     p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
                     p.avoid1 = FALSE, n = FALSE, se = FALSE) {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings.")
  }
  if (!is.character(yname)) {
    stop("For yname input, please enter character string")
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
  if (!is.logical(se)) {
    stop("For se input, please enter TRUE or FALSE")
  }
  
  # Drop missing values
  locs.complete <- which(!is.na(x) & !is.na(y))
  x <- x[locs.complete]
  y <- y[locs.complete]
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # Get unique values of x
  xvals <- sort(unique(x))
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- xvals
  }
  
  # Initialize table
  tbl <- matrix("", nrow = 1, ncol = length(xlevels)+4)
  
  # Get means and SD's or SE's and add variable name and 1st cell entry to table
  means <- tapply(X = y, INDEX = x, FUN = mean)
  ns <- tapply(X = y, INDEX = x, FUN = length)
  if (se == FALSE) {
    vars <- tapply(X = y, INDEX = x, FUN = sd)
    tbl[1, 1] <- paste(yname, ", M (SD)", sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- paste(sprintf(spf, mean(y[!is.na(x)])), " (", sprintf(spf, sd(y[!is.na(x)])), ")", sep = "")
  } else {
    vars <- tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
    tbl[1, 1] <- paste(yname, ", M (SE)", sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- paste(sprintf(spf, mean(y[!is.na(x)])), " (", sprintf(spf, sd(y[!is.na(x)])/sqrt(length(y[!is.na(x)]))), ")", sep = "")
  }
  
  # Add mean (SD/SE) values to table
  tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means), " (", sprintf(spf, vars), ")", sep = "")
  
  # Add p-value based on ANOVA or t-test depending on number of levels of x
  if (length(xlevels) == 2) {
    
    # F test for equal variances then appropriate t-test
    f <- var.test(x = y[x == xvals[1]], y = y[x == xvals[2]])
    if (f$p.value < 0.05) {
      p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]], var.equal = FALSE)$p.value
      print(paste("Unequal variance t-test was used to compare mean ", yname, " in the two groups.", sep = ""))
    } else {
      p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]], var.equal = TRUE)$p.value
      print(paste("Equal variance t-test was used to compare mean ", yname, " in the two groups.", sep = ""))
    }
    
  } else {
    
    # ANOVA
    p <- anova(lm(y ~ as.factor(x)))$"Pr(>F)"[1]
    print(paste("ANOVA was used to compare means for ", yname, sep = ""))
    
  }
  
  # Add p-value from t-test
  tbl[1, ncol(tbl)] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)
  
  # Add column names
  colnames(tbl) <- c("Variable", "N", "Overall", xlevels, "P")
  
  # Drop N column if requested
  if (n == FALSE) {
    tbl <- tbl[, -which(colnames(tbl) == "N"), drop = FALSE]
  }
  
  # If latex is TRUE, do some re-formatting
  if (latex == TRUE) {
    plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
    if (length(plocs) > 0) {
      tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
    }
  }
  
  # Return table
  return(tbl)
  
}