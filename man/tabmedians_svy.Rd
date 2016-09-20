\name{tabmedians.svy}
\alias{tabmedians.svy}
\title{
Generate Summary Tables of Median Comparisons for Statistical Reports (Survey Data)
}
\description{
This function compares the median of a continuous variable across levels of a categorical variable and summarizes the results in a clean table for a statistical report. Similar to tabmeans, but for survey data. Relies heavily on the 'survey' package [1,2].
}
\usage{
tabmedians.svy(svy, x, y, latex = FALSE, xlevels = NULL, yname = "Y variable", 
               test = "wilcoxon", decimals = 1, p.include = TRUE, p.decimals = c(2, 3),
               p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
               n.column = FALSE, n.headings = TRUE, parenth = "iqr", text.label = NULL, 
               parenth.sep = "-", bold.colnames = TRUE, bold.varnames = FALSE, 
               variable.colname = "Variable")
}
\arguments{
  \item{svy}{
Survey design object created by a call to svydesign [1,2]. 
}
  \item{x}{
Vector of values for the categorical variable.
}
  \item{y}{
Vector of values for the continuous variable.
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [3]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Optional label for the continuous variable.
}
  \item{test}{
Controls statistical test. Must be a possible value for the 'test' input of the svyranktest function in the survey package [1,2]: 'wilcoxon' for Mann-Whitney U/Wilcoxon test of whether one group is from distribution that is stochastically greater than the other; 'vanderWaerden' for  Van der Waerden test of whether the population distribution functions are equal; 'median' for Mood's test for whether the population medians are equal; and 'KruskalWallis' for Kruskal-Wallis test which is Mann-Whitney U/Wilcoxon generalized to three or more groups.
}
  \item{decimals}{
Number of decimal places for means and standard deviations or standard errors.
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
  \item{n.column}{
If TRUE, the table will have a column for (unweighted) sample size.
}
  \item{n.headings}{
If TRUE, the table will indicate the (unweighted) sample size overall and in each group in parentheses after the column headings.
}
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the medians in each cell. Possible choices are as follows: 'minmax' for minimum and maximum; 'range' for difference between minimum and maximum; 'q1q3' for first and third quartiles; 'iqr' for difference between first and third quartiles; or 'none' for no parentheses at all.
}
  \item{text.label}{
Optional text to put after the variable name. For example, if parenth is 'q1q3' and yname is 'BMI' the default label would be 'BMI, Median (Q1-Q3)'. You might prefer to set text.label to something like 'Med (Quartile 1-Quartile 3)' instead.
}
  \item{parenth.sep}{
Optional character specifying the separator for the two numbers in parentheses when parenth is set to 'minmax' or 'q1q3'. The default is a dash, so values in the table are formatted as Median (Lower-Upper). If you set parenth.sep to ', ' the values in the table would instead be formatted as Median (Lower, Upper).
}
  \item{bold.colnames}{
If TRUE, column headings are printed in bold font. Only applies if latex = TRUE. 
}
  \item{bold.varnames}{
If TRUE, variable name in the first column of the table is printed in bold font. Only applies if latex = TRUE.
}
  \item{variable.colname}{
Character string with desired heading for first column of table, which shows the y variable name.
}
}
\details{
NA
}
\value{
A character matrix with the requested table comparing median y across levels of x. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [3].
}
\references{
1. Lumley T (2012). survey: analysis of complex survey samples. R package version 3.28-2, \url{https://cran.r-project.org/package=survey}.

2. Lumley T (2014). Analysis of complex survey samples. Journal of Statistical Software 9(1): 1-19.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{https://cran.r-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{svydesign}},
\code{\link{svyquantile}},
\code{\link{svyranktest}},
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmulti}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans.svy}},
\code{\link{tabmulti.svy}},
\code{\link{tabglm.svy}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ median }
\keyword{ survey }