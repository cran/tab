\name{tabmulti}
\alias{tabmulti}
\title{
Generate Multi-row Tables Comparing Means/Medians/Frequencies of Multiple Variables Across Levels of One Categorical Variable
}
\description{
This function basically provides an alternative to making multiple calls to tabmeans, tabmedians, and tabfreq, then using rbind to combine the results into a single table.
}
\usage{
tabmulti(dataset, xvarname, yvarnames, ymeasures = NULL, listwise.deletion = TRUE,
         latex = FALSE, xlevels = NULL, ynames = yvarnames, ylevels = NULL, 
         quantiles = NULL, quantile.vals = FALSE, parenth.sep = "-", decimals = NULL, 
         cell = "n", freq.parenth = NULL, freq.text.label = NULL, freq.tests = "chi", 
         means.parenth = "sd", means.text.label = NULL, variance = "unequal", 
         medians.parenth = "iqr", medians.text.label = NULL, p.include = TRUE, 
         p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
         p.avoid1 = FALSE, overall.column = TRUE, n.column = FALSE, n.headings = TRUE, 
         compress = FALSE, bold.colnames = TRUE, bold.varnames = FALSE, 
         bold.varlevels = FALSE, variable.colname = "Variable")
}
\arguments{
  \item{dataset}{
Data frame or matrix containing variables of interest.
}
  \item{xvarname}{
Character string with name of x (column) variable. Should be one of colnames(dataset).
}
  \item{yvarnames}{
Character string or vector of character strings with names of y (row) variables. Each element should be one of colnames(dataset).
}
  \item{ymeasures}{
Character string or vector of character strings indicating whether each y variable should be summarized by mean, median, or frequency. For example, if yvarnames has length three and you wish to display frequencies for the first variable, means for the second, and medians for the third, you would set ymeasures to c("freq", "mean", "median"). If unspecified, function displays frequencies for any factor variable or numeric variable with five or fewer unique values, and means for numeric variables with more than five levels.
}
  \item{listwise.deletion}{
If TRUE, observations with missing values for any y variable are excluded entirely; if FALSE, all available data is used for each comparison. If FALSE, recommend also setting n to TRUE so table shows effective sample size for each comparison.
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [1]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x, used in the column headings. If unspecified, the function uses the values that x takes on.
}
  \item{ynames}{
Optional labels for the y variables. If unspecified, y variable names are used.
}
  \item{ylevels}{
Character vector or list of character vectors to label the levels of the categorical y variables.
}
  \item{quantiles}{
If specified, function compares y variables across quantiles of the x variable. For example, if x contains continuous BMI values and y contains continuous HDL and race, setting quantiles to 3 would result in mean HDL and distribution of race being compared across tertiles of BMI.
}
  \item{quantile.vals}{
If TRUE, labels for x show quantile number and corresponding range of the x variable. For example, Q1 [0.00, 0.25). If FALSE, labels for quantiles just show quantile number (e.g. Q1). Only used if xlevels is not specified.
}
  \item{parenth.sep}{
Optional character specifying the separator between first and second numbers in parentheses (e.g. lower and upper bound of confidence intervals, when requested). Usually either "-" or ", " depending on user preference.
}
    \item{decimals}{
Numeric value of vector of numeric values indicating how many decimal places should be used in reporting statistics for each y variable.
}
  \item{cell}{
Controls what values are placed in cells for frequency comparisons. Possible choices are "n" for counts, "tot.percent" for table percentage, "col.percent" for column percentage, "row.percent" for row percentage, "tot.prop" for table proportion, "col.prop" for column proportion, "row.prop" for row proportion, "n/totn" for count/total counts, "n/coln" for count/column count, and "n/rown" for count/row count.
}
  \item{freq.parenth}{
Controls what values (if any) are placed in parentheses after the values in each cell for frequency comparisons. By default, if cell is "n", "n/totn", "n/coln", or "n/rown" then the corresponding percentage is shown in parentheses; if cell is "tot.percent", "col.percent", "row.percent", "tot.prop", "col.prop", or "row.prop" then a 95\% confidence interval for the requested percentage of proportion is shown in parentheses. Possible values are "none", "se" (for standard error of requested percentage or proportion based on cell), "ci" (for 95\% confidence interval for requested percentage of proportion based on cell), "tot.percent", "col.percent", "row.percent", "tot.prop", "col.prop", and "row.prop".
}
  \item{freq.text.label}{
Optional text to put after the y variable name for frequency comparisons, identifying what cell values and parentheses indicate in the table. If unspecified, function uses default labels based on cell and freq.parenth settings. Set to "none" for no text labels.
}
  \item{freq.tests}{
Character string or vector of character strings indicating what statistical tests should be used to compare distributions of each categorical row variable across levels of the column variable. Elements can be "chi" for Pearson's chi-squared test, which is valid only in large samples; 'fisher' for Fisher's exact test, which is valid in small or large samples; 'z' for z test without continuity correction; or 'z.continuity' for z test with continuity correction. 'z' and 'z.continuity' can only be used for binary column and row variables.
}
  \item{means.parenth}{
Controls what values (if any) are placed in parentheses after the means in each cell for mean comparisons. Possible values are "none", "sd" for standard deviation, "se" for standard error, "t.ci" for 95\% confidence interval for population mean based on t distribution, and "z.ci" for 95\% confidence interval for population mean based on z distribution.
}
  \item{means.text.label}{
Optional text to put after the y variable name for mean comparisons, identifying what cell values and parentheses indicate in the table. If unspecified, function uses default labels based on parenth, e.g. M (SD) if means.parenth is "sd". Set to "none" for no text labels.
}
  \item{variance}{
Controls whether equal variance t-test or unequal variance t-test is used for mean comparisons when x has two levels. Possible values are "equal" for equal variance, "unequal" for unequal variance, or "ftest" for F test to determine which version of the t-test to use. Note that unequal variance t-test is less restrictive than equal variance t-test, and the F test is only valid when y is normally distributed in both x groups.
}
  \item{medians.parenth}{
Controls what values (if any) are placed in parentheses after the medians in each cell for median comparisons. Possible values are "none", "iqr" for difference between first and third quartiles, "range" for difference between minimum and maximum, "minmax" for minimum and maximum, and "q1q3" for first and third quartiles.
}
  \item{medians.text.label}{
Optional text to put after the y variable name for median comparisons, identifying what cell values and parentheses indicate in the table. If unspecified, function uses default labels based on parenth, e.g. Median (IQR) if medians.parenth is "iqr". Set to "none" for no text labels.
}
  \item{p.include}{
If FALSE, statistical test is not performed and p-value is not returned. 
}
  \item{p.decimals}{
Number of decimal places for p-values. If a vector is provided rather than a single value, number of decimal places will depend on what range the p-value lies in. See p.cuts.
}
  \item{p.cuts}{
Cut-point(s) to control number of decimal places used for p-values. For example, by default p.cuts is 0.1 and p.decimals is c(2, 3). This means that p-values in the range [0.1, 1] will be printed to two decimal places, while p-values in the range [0, 0.1) will be printed to three decimal places.
}
  \item{p.lowerbound}{
Controls cut-point at which p-values are no longer printed as their value, but rather <lowerbound. For example, by default p.lowerbound is 0.001. Under this setting, p-values less than 0.001 are printed as <0.001.
}
  \item{p.leading0}{
If TRUE, p-values are printed with 0 before decimal place; if FALSE, the leading 0 is omitted.
}
  \item{p.avoid1}{
If TRUE, p-values rounded to 1 are not printed as 1, but as >0.99 (or similarly depending on values for p.decimals and p.cuts). 
}
  \item{overall.column}{
If FALSE, column showing frequencies/means/medians for y in full sample is suppressed.
}
  \item{n.column}{
If TRUE, the table will have a column for (unweighted) sample size.
}
  \item{n.headings}{
If TRUE, the table will indicate the (unweighted) sample size overall and in each group in parentheses after the column headings.
}
  \item{compress}{
Logical indicating whether categorical y variables with two levels should be compressed into a single row rather than two rows for the table.
}
  \item{bold.colnames}{
If TRUE, column headings are printed in bold font. Only applies if latex = TRUE. 
}
  \item{bold.varnames}{
If TRUE, variable name in the first column of the table is printed in bold font. Only applies if latex = TRUE.
}
  \item{bold.varlevels}{
If TRUE, levels of categorical y variables are printed in bold font. Only applies if latex = TRUE.
}
  \item{variable.colname}{
Character string with desired heading for first column of table, which shows the y variable name and levels.
}
}
\details{
See help files for tabmeans, tabmedians, and tabfreq for details on statistical tests.
}
\value{
A character matrix comparing mean/medians/frequencies of row variables across levels of the column variable. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
}
\references{
1. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmedians}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans.svy}},
\code{\link{tabmedians.svy}},
\code{\link{tabmulti.svy}},
\code{\link{tabglm.svy}}
}
\examples{
# Load in sample dataset d
data(d)

# Compare age, sex, race, and BMI in control vs. treatment group
# data for each comparison
table1 <- tabmulti(dataset = d, xvarname = "Group", 
                   yvarnames = c("Age", "Sex", "Race", "BMI"))
                   
# Repeat, but use all available data for each comparison (as opposed to listwise deletion)
table2 <- tabmulti(dataset = d, xvarname = "Group", n.column = TRUE, n.headings = FALSE,
                   yvarnames = c("Age", "Sex", "Race", "BMI"), listwise.deletion = FALSE)
                   
# Same as table1, but compare medians rather than means for BMI
table3 <- tabmulti(dataset = d, xvarname = "Group", 
                   yvarnames = c("Age", "Sex", "Race", "BMI"), 
                   ymeasures = c("mean", "freq", "freq", "median"))

# Click on table1, table2, or table3 in the Workspace tab of RStudio to see the tables 
# that could be copied and pasted into a report or manuscript. Alternatively, setting 
# the latex input to TRUE produces tables that can be inserted into LaTeX using the 
# xtable package.
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }