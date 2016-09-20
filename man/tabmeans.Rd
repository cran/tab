\name{tabmeans}
\alias{tabmeans}
\title{
Generate Summary Tables of Mean Comparisons for Statistical Reports 
}
\description{
This function compares the mean of a continuous variable across levels of a categorical variable and summarizes the results in a clean table (or figure) for a statistical report.
}
\usage{
tabmeans(x, y, latex = FALSE, variance = "unequal", xname = NULL, xlevels = NULL, 
         yname = NULL, quantiles = NULL, quantile.vals = FALSE, parenth = "sd", 
         text.label = NULL, parenth.sep = "-", decimals = NULL, p.include = TRUE, 
         p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE,
         p.avoid1 = FALSE, overall.column = TRUE, n.column = FALSE, n.headings = TRUE,
         bold.colnames = TRUE, bold.varnames = FALSE, variable.colname = "Variable", 
         fig = FALSE, fig.errorbars = "z.ci", fig.title = NULL, print.html = FALSE,
         html.filename = "table1.html")
}
\arguments{
  \item{x}{
Vector of values for the categorical x variable.
}
  \item{y}{
Vector of values for the continuous y variable.
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [1]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{variance}{
Controls whether equal variance t-test or unequal variance t-test is used when x has two levels. Possible values are "equal" for equal variance, "unequal" for unequal variance, or "ftest" for F test to determine which version of the t-test to use. Note that unequal variance t-test is less restrictive than equal variance t-test, and the F test is only valid when y is normally distributed in both x groups.
}
  \item{xname}{
Label for the categorical variable. Only used if fig is TRUE.
}
  \item{xlevels}{
Optional character vector to label the levels of x, used in the column headings. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Optional label for the continuous y variable. If unspecified, variable name of y is used.
}
  \item{quantiles}{
If specified, function compares means of the y variable across quantiles of the x variable. For example, if x contains continuous BMI values and y contains continuous HDL cholesterol levels, setting quantiles to 3 would result in mean HDL being compared across tertiles of BMI.
}
  \item{quantile.vals}{
If TRUE, labels for x show quantile number and corresponding range of the x variable. For example, Q1 [0.00, 0.25). If FALSE, labels for quantiles just show quantile number (e.g. Q1). Only used if xlevels is not specified.
}
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the means in each cell. Possible values are "none", "sd" for standard deviation, "se" for standard error, "t.ci" for 95\% confidence interval for population mean based on t distribution, and "z.ci" for 95\% confidence interval for population mean based on z distribution.
}
  \item{text.label}{
Optional text to put after the y variable name, identifying what cell values and parentheses indicate in the table. If unspecified, function uses default labels based on parenth, e.g. M (SD) if parenth is "sd". Set to "none" for no text labels.
}
  \item{parenth.sep}{
Optional character specifying the separator between lower and upper bound of confidence interval (when requested). Usually either "-" or ", " depending on user preference.
}
  \item{decimals}{
Number of decimal places for means and standard deviations/standard errors/confidence intervals. If unspecified, function uses 0 decimal places if the largest mean (in magnitude) is in [1,000, Inf), 1 decimal place if [10, 1,000), 2 decimal places if [0.1, 10), 3 decimal places if [0.01, 0.1), 4 decimal places if [0.001, 0.01), 5 decimal places if [0.0001, 0.001), and 6 decimal places if [0, 0.0001).
}
  \item{p.include}{
If FALSE, t-test is not performed and p-value is not returned. 
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
If FALSE, column showing mean of y in full sample is suppressed.
}
  \item{n.column}{
If TRUE, the table will have a column for (unweighted) sample size.
}
  \item{n.headings}{
If TRUE, the table will indicate the (unweighted) sample size overall and in each group in parentheses after the column headings.
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
  \item{fig}{
If TRUE, a figure is returned rather than a table. The figure shows mean (95 percent confidence interval) for each level of x.
}
  \item{fig.errorbars}{
Controls error bars around mean when fig is TRUE. Possible values are "sd" for +/- 1 standard deviation, "se" for +/- 1 standard error, "t.ci" for 95\% confidence interval based on t distribution, "z.ci" for 95\% confidence interval based on z distribution, and "none" for no error bars.
}
  \item{fig.title}{
Title of figure. If unspecified, title is set to "Mean yname by xname".
}
  \item{print.html}{
If TRUE, function prints a .html file to the current working directory.
}
  \item{html.filename}{
Character string indicating the name of the .html file that gets printed if print.html is set to TRUE.
}
}
\details{
If x has two levels, a t-test is used to test for a difference in means. If x has more than two levels, a one-way analysis of variance is used to test for a difference in means across the groups.

Both x and y can have missing values. The function drops observations with missing x or y. 
}
\value{
A character matrix with the requested table comparing mean y across levels of x. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
}
\references{
1. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{https://cran.r-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
In older versions of RStudio, it was easier to copy tables from the Viewer and paste them directly into a text editor. The Viewer changed a few versions ago, and now it seems to work better if you paste into Microsoft Excel, and then copy again and paste into Microsoft Word. This is a little clumsy, so I recently added the new option to print a .html file with the table to your current working directory (see function inputs print.html and html.filename). Copying and pasting from the table from the .html file into a text editor seems to work well.

If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{tabfreq}},
\code{\link{tabmedians}},
\code{\link{tabmulti}},
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
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Compare mean BMI in control group vs. treatment group - table and figure
meanstable1 <- tabmeans(x = d$Group, y = d$BMI)
meansfig1 <- tabmeans(x = d$Group, y = d$BMI, fig = TRUE)

# Compare mean BMI by race - table and figure
meanstable2 <- tabmeans(x = d$Race, y = d$BMI)
meansfig2 <- tabmeans(x = d$Race, y = d$BMI, fig = TRUE)

# Compare mean baseline systolic BP across tertiles of BMI - table and figure
meanstable3 <- tabmeans(x = d$BMI, y = d$bp.1, yname = "Systolic BP", quantiles = 3)
meansfig3 <- tabmeans(x = d$BMI, y = d$bp.1, quantiles = 3, fig = TRUE, 
                      yname = "Systolic BP", xname = "BMI Tertile")

# Create single table comparing mean BMI and mean age in control vs. treatment group
meanstable4 <- rbind(tabmeans(x = d$Group, y = d$BMI), tabmeans(x = d$Group, y = d$Age))
                     
# An easier way to make the above table is to call the tabmulti function
meanstable5 <- tabmulti(dataset = d, xvarname = "Group", yvarnames = c("BMI", "Age"))
                        
# meanstable4 and meanstable5 are equivalent
all(meanstable4 == meanstable5)

# Click on meanstable 1, ... , meanstable5 in the Workspace tab of RStudio to see the 
# tables that could be copied and pasted into a report. Alternatively, setting the latex 
# input to TRUE produces tables that can be inserted into LaTeX using the xtable package.
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }