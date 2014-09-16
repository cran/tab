tabmulti <- function(dataset, xvarname, yvarnames, ymeasures = NULL, listwise.deletion = FALSE,
                     latex = FALSE, xlevels = NULL, ynames = yvarnames, ylevels = NULL, 
                     freq.tests = "chi", decimals = 1, p.decimals = c(2, 3), p.cuts = 0.01,
                     p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE, 
                     se = FALSE, compress = FALSE, parenth = "iqr", text.label = NULL, 
                     parenth.sep = "-") {
  
  # If any inputs are not correct class, return error
  if (!is.matrix(dataset) & !is.data.frame(dataset)) {
    stop("For dataset input, please enter matrix or data frame with variables of interest")
  }
  if (!is.character(xvarname)) {
    stop("For xvarname input, please enter character string with name of column variable")
  }
  if (!all(is.character(yvarnames))) {
    stop("For yvarnames input, please enter character string or vector of character strings with name(s) of row variable(s)")
  }
  if (!is.null(ymeasures) && !all(ymeasures %in% c("mean", "median", "freq"))) {
    stop("For ymeasures input, please enter character string or vector of character strings of same length as yvarnames")
  }
  if (!is.logical(listwise.deletion)) {
    stop("For listwise.deletion input, please enter TRUE or FALSE")
  }
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
  }
  if (!all(is.character(ynames))) {
    stop("For ynames input, please enter character string or vector of character strings of same length as yvarnames")
  }
  if (!is.null(ylevels) && !all(unlist(lapply(X = ylevels, FUN = function(x) all(is.character(x)))))) {
    stop("For ylevels input, please enter vector or list of vectors of character strings")
  }
  if (!all(freq.tests %in% c("chi", "fisher", "z", "z.continuity"))) {
    stop("For freq.tests input, please enter character string or vector of character strings indicating what statistical test
         should be performed for each categorical row variable. Each element should be 'chi', 'fisher', 'z', or 'z.continuity'")
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
  if (!is.logical(compress)) {
    stop("For compress input, please enter TRUE or FALSE")
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
  
  # If listwise.deletion is TRUE, drop observations with missing values for column variable or any row variables
  if (listwise.deletion == TRUE){
    
    dataset <- dataset[which(!is.na(dataset[, xvarname])), ]
    for (ii in 1:length(yvarnames)) {
      dataset <- dataset[which(!is.na(dataset[, yvarnames[ii]])), ]
    }
    
  }
  
  # If ymeasures is single value, create vector of repeat values
  if (length(ymeasures) == 1) {
    ymeasures <- rep(ymeasures, length(yvarnames))
  }
  
  # If freq.tests is a single value, create vector of repeat values
  if (length(freq.tests) == 1) {
    freq.tests <- rep(freq.tests, length(yvarnames))
  }
  
  # If ymeasures is NULL, guess what measures are appropriate based on each variable
  if (is.null(ymeasures)) {
    ymeasures <- c()
    for (ii in 1:length(yvarnames)) {
      if (is.factor(dataset[, yvarnames[ii]]) | length(unique(dataset[!is.na(dataset[, yvarnames[ii]]), yvarnames[ii]])) <= 5) {
        ymeasures <- c(ymeasures, "freq")
      } else
        ymeasures <- c(ymeasures, "mean")
    }
  }
  
  # Call tabmeans, tabmedians, or tabfreq repeatedly
  freqindex <- 0
  for (ii in 1:length(yvarnames)) {
    if (ymeasures[ii] == "mean") {
      current <- tabmeans(x = dataset[, xvarname], y = dataset[, yvarnames[ii]], latex = latex, xlevels = xlevels,
                          yname = ynames[ii], decimals = decimals, p.decimals = p.decimals, p.cuts = p.cuts,
                          p.lowerbound = p.lowerbound, p.leading0 = p.leading0, p.avoid1 = p.avoid1,
                          n = n, se = se)
    } else if (ymeasures[ii] == "median") {
      current <- tabmedians(x = dataset[, xvarname], y = dataset[, yvarnames[ii]], latex = latex, xlevels = xlevels,
                            yname = ynames[ii], decimals = decimals, p.decimals = p.decimals, p.cuts = p.cuts,
                            p.lowerbound = p.lowerbound, p.leading0 = p.leading0, p.avoid1 = p.avoid1,
                            n = n, parenth = parenth, text.label = text.label, parenth.sep = parenth.sep)
    } else if (ymeasures[ii] == "freq") {
      freqindex <- freqindex + 1
      current <- tabfreq(x = dataset[, xvarname], y = dataset[, yvarnames[ii]], latex = latex, xlevels = xlevels,
                         yname = ynames[ii], ylevels = ylevels[[freqindex]], test = freq.tests[ii], 
                         decimals = decimals, p.decimals = p.decimals, p.cuts = p.cuts, p.lowerbound = p.lowerbound,
                         p.leading0 = p.leading0, p.avoid1 = p.avoid1, n = n, compress = compress)
    }
    if (ii == 1) {
      results <- current
    } else {
      results <- rbind(results, current)
    }
  }
  
  # Return results matrix
  return(results)
  
}