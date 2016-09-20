\name{tabmulti.svy}
\alias{tabmulti.svy}
\title{
Generate Multi-row Tables Comparing Means/Medians/Frequencies of Multiple Variables Across Levels of One Categorical Variable (Survey Data)
}
\description{
This function basically provides an alternative to making multiple calls to tabmeans.svy, tabmedians.svy, and tabfreq.svy, then using rbind to combine the results into a single table. Similar to tabmulti, but for survey data. Relies heavily on the 'survey' package [1,2].
}
\usage{
tabmulti.svy(svy, xvarname, yvarnames, ymeasures = NULL, listwise.deletion = FALSE, 
             latex = FALSE, xlevels = NULL, ynames = yvarnames, ylevels = NULL, 
             mean.tests = "Wald", median.tests = "wilcoxon", freq.tests = "F", 
             decimals = 1, p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01, 
             p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n.column = FALSE, 
             n.headings = TRUE, se = FALSE, compress = FALSE, parenth = "iqr", 
             text.label = NULL, parenth.sep = "-", bold.colnames = TRUE, 
             bold.varnames = FALSE, bold.varlevels = FALSE, variable.colname = "Variable")
}
\arguments{
  \item{svy}{
Survey design object created by a call to svydesign [1,2]. 
}
  \item{xvarname}{
Character string with name of column variable. Should be one of colnames(dataset).
}
  \item{yvarnames}{
Character string or vector of character strings with names of row variables. Each element should be one of colnames(dataset).
}
  \item{ymeasures}{
Character string or vector of character strings indicating whether each row variable should be summarized by mean, median, or frequency. For example, if yvarnames has length three and you wish to display frequencies for the first variable, means for the second, and medians for the third, you would set ymeasures to c("freq", "mean", "median"). If unspecified, function displays frequencies for any factor variable or numeric variable with five or fewer unique values, and means for numeric variables with more than five levels.
}
  \item{listwise.deletion}{
If TRUE, observations with missing values for any row variable are excluded entirely; if FALSE, all available data is used for each comparison. If FALSE, recommend also setting n to TRUE so table shows effective sample size for each comparison.
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [3]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses the values that x takes on.
}
  \item{ynames}{
Optional labels for the row variables.
}
  \item{ylevels}{
Character vector or list of character vectors to label the levels of the categorical row variables.
}
  \item{mean.tests}{
Character string or vector of character strings indicating what statistical tests should be used to compare means for each row variable for which a comparison of means is requested. Elements should be 'Wald' for Wald test or 'LRT' for likelihood ratio test.
}
  \item{median.tests}{
Character string or vector of character strings indicating what statistical tests should be used to compare medians for each row variable for which a comparison of medians is requested. Elements should be possible values for the 'test' input of the svyranktest function in the survey package [1,2]: 'wilcoxon' for Mann-Whitney U/Wilcoxon test of whether one group is from distribution that is stochastically greater than the other; 'vanderWaerden' for  Van der Waerden test of whether the population distribution functions are equal; 'median' for Mood's test for whether the population medians are equal; and 'KruskalWallis' for Kruskal-Wallis test which is Mann-Whitney U/Wilcoxon generalized to three or more groups.
}
  \item{freq.tests}{
Character string or vector of character strings indicating what statistical tests should be used to compare distributions of each categorical row variable across levels of the column variable. Elements should be possible values for the 'statistic' input of the svychisq function in the survey package [1,2]: 'F', 'Chisq', 'Wald', 'adjWald', 'lincom', or 'saddlepoint'.
}
  \item{decimals}{
Number of decimal places for various cell entries, such as means and percentages. Does not affect p-values.
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
  \item{se}{
If TRUE, the table will present mean (standard error) rather than mean (standard deviation) for continuous row variables.
}
  \item{compress}{
If TRUE, categorical row variables with two levels will have a single row for n (percent) for the higher level. For example, if a row variable is sex, with 0 for females and 1 for males, setting compress = TRUE would result in the sex row showing n (percent) for males only. If FALSE, the table would have two rows for sex, one showing n (percent) for males and another sohwing n (percent) for females.
}
  \item{parenth}{
For median comparisons, controls what values (if any) are placed in parentheses after the medians in each cell. Possible choices are as follows: 'minmax' for minimum and maximum; 'range' for difference between minimum and maximum; 'q1q3' for first and third quartiles; 'iqr' for difference between first and third quartiles; or 'none' for no parentheses at all.
}
  \item{text.label}{
For median comparisons, optional text to put after the variable name. For example, if parenth is 'q1q3' and yname is 'BMI' the default label would be 'BMI, Median (Q1-Q3)'. You might prefer to set text.label to something like 'Med (Quartile 1-Quartile 3)' instead.
}
  \item{parenth.sep}{
For median comparisons, optional character specifying the separator for the two numbers in parentheses when parenth is set to 'minmax' or 'q1q3'. The default is a dash, so values in the table are formatted as Median (Lower-Upper). If you set parenth.sep to ', ' the values in the table would instead be formatted as Median (Lower, Upper).
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
Please see help files for tabmeans.svy, tabmedians.svy, and tabfreq.svy for details on statistical tests.
}
\value{
A character matrix comparing mean/medians/frequencies of row variables across levels of the column variable. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [3].
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
\code{\link{svyglm}},
\code{\link{svychisq}},
\code{\link{svyquantile}},
\code{\link{svyranktest}},
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmedians}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans.svy}},
\code{\link{tabmedians.svy}},
\code{\link{tabglm.svy}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }