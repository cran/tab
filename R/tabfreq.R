tabfreq <- function(x, y, latex = FALSE, xlevels = NULL, yname = "Y variable", ylevels = NULL, 
                    test = "chi", decimals = 1, p.decimals = c(2, 3), p.cuts = 0.01,
                    p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE, 
                    compress = FALSE) {
  
  # Get cell counts and proportions
  counts <- table(y, x)
  props <- 100*prop.table(counts)
  colprops <- 100*prop.table(counts, 2)
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
  }
  if (!is.character(yname)) {
    stop("For yname input, please enter character string")
  }
  if (!is.null(ylevels) && !is.character(ylevels)) {
    stop("For ylevels input, please enter vector of character strings")
  }
  if (! test %in% c("chi", "fisher", "z", "z.continuity")) {
    stop("For test input, please enter 'chi', 'fisher', 'z', or 'z.continuity'")
  }
  if (test %in% c("z", "z.continuity") & ! all(dim(counts) == 2)) {
    stop("For test input, 'z' and 'z.continuity' can only be used if both x and y are binary")
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
  if (!is.logical(compress)) {
    stop("For compress input, please enter TRUE or FALSE")
  }
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels <- rownames(counts)
  }
  
  # Initialize table
  tbl <- matrix("", nrow = nrow(counts)+1, ncol = ncol(counts)+4) 
  
  # Add variable name and levels of Y to first row
  tbl[, 1] <- c(paste(yname, ", n (%)", sep = ""), paste("  ", ylevels, sep = ""))
  
  # Add N column
  tbl[1, 2] <- sprintf("%.0f", sum(counts))
  
  # n (%) in each level of y
  tbl[2:nrow(tbl), 3] <- paste(sprintf("%.0f", rowSums(counts)), " (", sprintf(spf, rowSums(props)), ")", sep = "")
  
  # n (%) for each cell
  for (i in 1:nrow(counts)) {
    for (j in 1:ncol(counts)) {
      tbl[i+1, j+3] <- paste(sprintf("%.0f", counts[i, j]), " (", sprintf(spf, colprops[i, j]), ")", sep = "")
    }
  }
  
  # Statistical test
  if (nrow(counts) == 1) {
    pval = "-"
  } else {
    if (test == "chi") {
      pval <- chisq.test(x = x, y = y)$p.value
      print(paste("Pearson's chi-square test was used to test whether the distribution of ", yname, " differed across groups.", sep = ""))
    } else if (test == "fisher") {
      pval <- fisher.test(x = x, y = y)$p.value
      print(paste("Fisher's exact test was used to test whether the distribution of ", yname, " differed across groups.", sep = ""))
    } else if (test == "z") {
      pval <- prop.test(x = counts, correct = FALSE)$p.value
      print(paste("A z-test (without continuity correction) was used to test whether proportions of ", yname, " differed in the two groups.", sep = ""))
    } else if (test == "z.continuity") {
      pval <- prop.test(x = counts)$p.value
      print(paste("A z-test (with continuity correction) was used to test whether proportions of ", yname, " differed in the two groups.", sep = ""))
    }
  }
  tbl[1, ncol(tbl)] <- formatp(p = pval, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)
  
  # If y binary and compress is TRUE, compress table to a single row
  if (nrow(counts) <= 2 & compress == TRUE) {
    tbl <- matrix(c(tbl[1, 1:2], tbl[nrow(tbl), 3:(ncol(tbl)-1)], tbl[1, ncol(tbl)]), nrow = 1)
  }
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- colnames(counts)
  }
  
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
    spacelocs <- which(substr(tbl[, "Variable"], 1, 2) == "  ")
    if (length(spacelocs) > 0) {
      tbl[spacelocs, "Variable"] <- paste("\\hskip .3cm ", substring(tbl[spacelocs, "Variable"], 3), sep = "")
    }
    chars <- strsplit(tbl[, "Variable"], "")
    for (ii in 1:length(chars)) {
      percentlocs <- which(chars[[ii]] == "%")
      if (length(percentlocs) > 0) {
        chars[[ii]][percentlocs] <- "\\%"
      }
    }
    tbl[, "Variable"] <- sapply(chars, function(x) paste(x, sep = "", collapse = ""))
  }
  
  # Return table
  return(tbl)
  
}