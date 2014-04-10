tabfreq <- function(x, y, latex = FALSE, xlevels = NULL, ylevels = NULL, yname = "Y variable", 
                    test = "chi", decimals = 1, p.decimals = c(2,3), p.cuts = 0.01,
                    p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE, 
                    compress = FALSE) {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (! test %in% c("chi", "fisher")) {
    stop("For test input, please enter 'chi' or 'fisher'")
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
  
  # Get cell counts and proportions
  counts <- table(y, x)
  props <- 100*prop.table(counts)
  colprops <- 100*prop.table(counts, 2)
  
  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels <- rownames(counts)
  }
  
  # Initialize table
  tbl <- matrix("", nrow = nrow(counts)+1, ncol = ncol(counts)+3) 
  
  # Add variable name and levels of Y to first row
  tbl[,1] <- c(paste(yname, ", n (%)", sep = ""), paste("  ", ylevels, sep = ""))
  
  # n (%) in each level of y
  tbl[2:nrow(tbl),2] <- paste(sprintf("%.0f", rowSums(counts)), " (", sprintf("%.1f", rowSums(props)), ")", sep = "")
  
  # n (%) for each cell
  for (i in 1:nrow(counts)) {
    for (j in 1:ncol(counts)) {
      tbl[i+1,j+2] <- paste(sprintf("%.0f", counts[i,j]), " (", sprintf("%.1f", colprops[i,j]), ")", sep = "")
    }
  }
  
  # Statistical test
  if (nrow(counts) == 1) {
    pval = "-"
  } else {
    if (test == "chi") {
      pval <- chisq.test(x = x, y = y)$p.value
    } else if (test == "fisher") {
      pval = fisher.test(x = x, y = y)$p.value
    }
  }
  tbl[1,ncol(tbl)] <- formatp(p = pval, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)
  
  # If x binary and compress is TRUE, compress table to a single row
  if (nrow(counts) <= 2 & compress == TRUE) {
    tbl <- matrix(c(tbl[1,1], tbl[nrow(tbl), 2:4], tbl[1,5]), nrow = 1)
  }
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- colnames(counts)
  }
  
  # If requested, include n's in xlevels labels
  if (n == TRUE) {
    xlevels <- paste(xlevels," (n = ", colSums(counts), ")", sep = "")
    overall <- paste("Overall (n = ", sum(counts), ")", sep = "")
  } else {
    overall <- "Overall"
  }
  
  # Add column names
  colnames(tbl) <- c("Variable", overall, xlevels, "P")
  
  # If latex is TRUE, do some re-formatting
  if (latex == TRUE) {
    plocs = which(substr(tbl[,"P"],1,1) == "<")
    if (length(plocs) > 0) {
      tbl[plocs,"P"] = paste("$<$", substring(tbl[plocs,"P"],2), sep = "")
    }
    spacelocs = which(substr(tbl[,"Variable"],1,2) == "  ")
    if (length(spacelocs) > 0) {
      tbl[spacelocs,"Variable"] = paste("\\hskip .3cm ", substring(tbl[spacelocs,"Variable"], 3), sep = "")
    }
    chars = strsplit(tbl[,"Variable"], "")
    for (ii in 1:length(chars)) {
      percentlocs = which(chars[[ii]] == "%")
      if (length(percentlocs) > 0) {
        chars[[ii]][percentlocs] = "\\%"
      }
    }
    tbl[,"Variable"] = sapply(chars, function(x) paste(x, sep = "", collapse = ""))
  }
  
  # Return table
  return(tbl)
  
}