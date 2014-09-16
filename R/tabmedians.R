tabmedians <- function(x, y, latex = FALSE, xlevels = NULL, yname = "Y variable", decimals = 1,
                       p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
                       p.avoid1 = FALSE, n = FALSE, parenth = "iqr", text.label = NULL,
                       parenth.sep = "-") {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
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
  if (! parenth %in% c("minmax", "range", "q1q3", "iqr", "none")) {
    stop("For parenth input, please enter one of the following: 'minmax', 'range', 'q1q3', 'iqr', 'none'")
  }
  if (!is.null(text.label) && !is.character(text.label)) {
    stop("For text.label input, please enter something like 'Median (IQR)' or just leave it unspecified")
  }
  if (!is.character(parenth.sep)) {
    stop("For parenth.sep input, please enter a character string")
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
  
  # Get medians and values for parentheses, and add variable name to 1st cell entry to table
  medians <- tapply(X = y, INDEX = x, FUN = median)
  ns <- tapply(X = y, INDEX = x, FUN = length)
  if (parenth == "minmax") {
    parent1 <- tapply(X = y, INDEX = x, FUN = min)
    parent2 <- tapply(X = y, INDEX = x, FUN = max)
    parent <- paste(sprintf(spf, parent1), parenth.sep, sprintf(spf, parent2), sep = "")
    if (is.null(text.label)) {
      text.label <- "Median (Min-Max)"
    }
    tbl[1, 1] <- paste(yname, ", ", text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- paste(sprintf(spf, median(y)), " (", sprintf(spf, min(y)), parenth.sep, sprintf(spf, max(y)), ")", sep = "")
    tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, medians), " (", parent, ")", sep = "")
  } else if (parenth == "range") {
    parent1 <- tapply(X = y, INDEX = x, FUN = min)
    parent2 <- tapply(X = y, INDEX = x, FUN = max)
    parent <- paste(sprintf(spf, parent2 - parent1))
    if (is.na(text.label)) {
      text.label <- "Median (Range)"
    }
    tbl[1, 1] <- paste(yname, ", ", text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- paste(sprintf(spf, median(y)), " (", sprintf(spf, max(y) - min(y)), ")", sep = "")
    tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, medians), " (", parent, ")", sep = "")
  } else if (parenth == "q1q3") {
    parent1 <- tapply(X = y, INDEX = x, FUN = function(x) quantile(x, probs = 0.25))
    parent2 <- tapply(X = y, INDEX = x, FUN = function(x) quantile(x, probs = 0.75))
    parent <- paste(sprintf(spf, parent1), parenth.sep, sprintf(spf, parent2), sep = "")
    if (is.null(text.label)) {
      text.label <- "Median (Q1-Q3)"
    }
    tbl[1, 1] <- paste(yname, ", ", text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- paste(sprintf(spf, median(y)), " (", sprintf(spf, quantile(y, probs = 0.25)), parenth.sep, sprintf(spf, quantile(y, 0.75)), ")", sep = "")
    tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, medians), " (", parent, ")", sep = "")
  } else if (parenth == "iqr") {
    parent1 <- tapply(X = y, INDEX = x, FUN = function(x) quantile(x, probs = 0.25))
    parent2 <- tapply(X = y, INDEX = x, FUN = function(x) quantile(x, probs = 0.75))
    parent <- paste(sprintf(spf, parent2 - parent1))
    if (is.null(text.label)) {
      text.label <- "Median (IQR)"
    }
    tbl[1, 1] <- paste(yname, ", ", text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- paste(sprintf(spf, median(y)), " (", sprintf(spf, quantile(y, probs = 0.75) - quantile(y, probs = 0.25)), ")", sep = "")
    tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, medians), " (", parent, ")", sep = "")
  } else if (parenth == "none") {
    if (is.null(text.label)) {
      text.label <- "Median"
    }
    tbl[1, 1] <- paste(yname, ", ", text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
    tbl[1, 3] <- sprintf(spf, median(y))
    tbl[1, 4:(ncol(tbl)-1)] <- sprintf(spf, medians)
  }
  
  # Add p-value from statistical test depending on number of levels of x
  if (length(xlevels) == 2) {
    
    # Mann-Whitney U test a.k.a. Wilcoxon rank-sum test
    p <- wilcox.test(y ~ x)$p.value
    
  } else {
    
    # Kruskal-Wallis rank-sum test
    p <- kruskal.test(y ~ as.factor(x))$p.value
    
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