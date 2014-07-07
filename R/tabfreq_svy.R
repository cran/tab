tabfreq.svy <- function(x, y, svy, latex = FALSE, xlevels = NULL, ylevels = NULL, 
                        yname = "Y variable", test = "F", decimals = 1, p.decimals = c(2,3), 
                        p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
                        p.avoid1 = FALSE, n = FALSE, compress = FALSE) {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (! test %in% c("F", "Chisq", "Wald", "adjWald", "lincom", "saddlepoint")) {
    stop("For test input, please enter a possible value for the 'statistic' input of the 
         svychisq function in the survey package: 'F', 'Chisq', 'Wald', 'adjWald', 'lincom', 
         or 'saddlepoint'. See svychisq documentation for details.")
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
  
  # Save x and y as character strings
  xstring <- x
  ystring <- y
  
  # Extract vectors x and y
  x <- svy$variables[, xstring]
  y <- svy$variables[, ystring]
  
  # Update survey object to include y and x explicitly
  svy2 <- update(svy, y = y, x = x)
  
  # Drop missing values if present
  locs <- which(!is.na(x) & !is.na(y))
  if (length(locs) < nrow(svy2)) {
    svy2 <- subset(svy2, !is.na(x) & !is.na(y))
    x <- svy2$variables[, xstring]
    y <- svy2$variables[, ystring]
  }
  
  # Basic table to get levels of x and y
  counts <- table(y, x)
  
  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels <- rownames(counts)
  }
  
  # Initialize table
  tbl <- matrix("", nrow = nrow(counts)+1, ncol = ncol(counts)+4) 
  
  # Add variable name and levels of Y to first row
  tbl[, 1] <- c(paste(yname, ", n (%)", sep = ""), paste("  ", ylevels, sep = ""))
  
  # Add N column
  tbl[1, 2] = sprintf("%.0f", sum(counts))
  
  # n (%) for each cell
  for (ii in 1:nrow(counts)) {
    yval <- rownames(counts)[ii]
    totmean <- svymean(y == yval, design = svy2)
    tbl[ii+1, 3] <- paste(sprintf("%.0f", rowSums(counts)[ii]), " (", sprintf(spf, totmean*100), ")", sep = "")
    yval <- rownames(counts)[ii]
    levmeans <- svyby(~y == yval, by = ~x, FUN = svymean, design = svy2)
    tbl[ii+1, 4:(ncol(tbl)-1)] <- paste(sprintf("%.0f", counts[ii, ]), " (", sprintf(spf, levmeans$"y == yvalTRUE"*100), ")", sep = "")
  }
  
  # Statistical test
  if (nrow(counts) == 1) {
    pval <- "-"
  } else {
    pval <- svychisq(~y + x, design = svy2, statistic = test)$p.value
  }
  tbl[1, ncol(tbl)] <- formatp(p = pval, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)
  
  # If y binary and compress is TRUE, compress table to a single row
  if (nrow(counts) <= 2 & compress == TRUE) {
    tbl <- matrix(c(tbl[1, 1], tbl[nrow(tbl), 2:(ncol(tbl)-1)], tbl[1, ncol(tbl)]), nrow = 1)
  }
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- colnames(counts)
  }
  
  # Add column names
  colnames(tbl) <- c("Variable", "N", "Overall", xlevels, "P")
  
  # If y binary and compress is TRUE, compress table to a single row
  if (nrow(counts) <= 2 & compress == TRUE) {
    tbl <- matrix(c(tbl[1, 1:2], tbl[nrow(tbl), 3:(ncol(tbl)-1)], tbl[1, ncol(tbl)]), nrow = 1)
  }
  
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