\name{tabfreq}
\alias{tabfreq}
\title{
Generate Frequency Tables for Statistical Reports
}
\description{
This function creates an I-by-J frequency table and summarizes the results in a clean table for a statistical report.
}
\usage{
tabfreq(x, y, latex = FALSE, xlevels = NULL, yname = NULL, ylevels = NULL, 
        quantiles = NULL, quantile.vals = FALSE, cell = "n", parenth = NULL, 
        text.label = NULL, parenth.sep = "-", test = "chi", decimals = NULL, 
        p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, 
        p.leading0 = TRUE, p.avoid1 = FALSE, overall.column = TRUE, n.column = FALSE, 
        n.headings = TRUE, compress = FALSE, compress.val = NULL, bold.colnames = TRUE, 
        bold.varnames = FALSE, bold.varlevels = FALSE, variable.colname = "Variable",
        print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{x}{
Vector of values indicating group membership for columns of IxJ table. 
}
  \item{y}{
Vector of values indicating group membership for rows of IxJ table. 
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [1]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x, used in the column headings. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Optional label for the y (row) variable. If unspecified, variable name of y is used.
}
  \item{ylevels}{
Optional character vector to label the levels of y. If unspecified, the function uses the values that y takes on. Note that levels of y will be listed in the order that they appear when you run table(y, x).
}
  \item{quantiles}{
If specified, function compares distribution of the y variable across quantiles of the x variable. For example, if x contains continuous BMI values and y is race, setting quantiles to 3 would result in the distribution of race being compared across tertiles of BMI.
}
  \item{quantile.vals}{
If TRUE, labels for x show quantile number and corresponding range of the x variable. For example, Q1 [0.00, 0.25). If FALSE, labels for quantiles just show quantile number (e.g. Q1). Only used if xlevels is not specified.
}
  \item{cell}{
Controls what value is placed in each cell of the table. Possible choices are "n" for counts, "tot.percent" for table percentage, "col.percent" for column percentage, "row.percent" for row percentage, "tot.prop" for table proportion, "col.prop" for column proportion, "row.prop" for row proportion, "n/totn" for count/total counts, "n/coln" for count/column count, and "n/rown" for count/row count.
}
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the values in each cell. By default, if cell is "n", "n/totn", "n/coln", or "n/rown" then the corresponding percentage is shown in parentheses; if cell is "tot.percent", "col.percent", "row.percent", "tot.prop", "col.prop", or "row.prop" then a 95\% confidence interval for the requested percentage of proportion is shown in parentheses. Possible values are "none", "se" (for standard error of requested percentage or proportion based on cell), "ci" (for 95\% confidence interval for requested percentage of proportion based on cell), "tot.percent", "col.percent", "row.percent", "tot.prop", "col.prop", and "row.prop".
}
  \item{text.label}{
Optional text to put after the y variable name, identifying what cell values and parentheses indicate in the table. If unspecified, function uses default labels based on cell and parenth settings. Set to "none" for no text labels.
}
  \item{parenth.sep}{
Optional character specifying the separator between lower and upper bound of confidence interval (when requested). Usually either "-" or ", "" depending on user preference.
}
  \item{test}{
Controls test for association between x and y. Use "chi" for Pearson's chi-squared test, which is valid only in large samples; "fisher" for Fisher's exact test, which is valid in small or large samples; "z" for z test without continuity correction; or "z.continuity" for z test with continuity correction. "z" and "z.continuity" can only be used if x and y are binary.
}
  \item{decimals}{
Number of decimal places for values in table (no decimals are used for counts). If unspecified, function uses 1 decimal for percentages and 3 decimals for proportions.
}
  \item{p.include}{
If FALSE, statistical test is not performed and p-value is not returned. 
}
  \item{p.decimals}{
Number of decimal places for p-values. If a vector is provided rather than a single value, number of decimal places will depend on what range the p-value lies in. See p.cuts input.
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
If FALSE, column showing distribution of y in full sample is suppressed.
}
  \item{n.column}{
If TRUE, the table will have a column for sample size.
}
  \item{n.headings}{
If TRUE, the table will indicate the sample size overall and in each group in parentheses after the column headings.
}
  \item{compress}{
If y has only two levels, setting compress to TRUE will produce a single row rather than two rows. For example, if y is sex with 0 for female, 1 for male, and cell = "n" and parenth = "col.pecent", setting compress = TRUE will return a table with n (percent) for males only. If FALSE, the table would show n (percent) for both males and females, which is somewhat redundant.
}
  \item{compress.val}{
When x and y are both binary, and compress is TRUE, compress.val can be used to specify which level of the y variable should be shown. For example, if x is sex and y is obesity status with levels "Obese" and "Not Obese", setting compress to TRUE and compress.val to "Not Obese" would result in the table comparing the proportions of subjects that are not obese by sex.
}
  \item{bold.colnames}{
If TRUE, column headings are printed in bold font. Only applies if latex = TRUE. 
}
  \item{bold.varnames}{
If TRUE, variable name in the first column of the table is printed in bold font. Only applies if latex = TRUE.
}
  \item{bold.varlevels}{
If TRUE, levels of the y variable are printed in bold font. Only applies if latex = TRUE.
}
  \item{variable.colname}{
Character string with desired heading for first column of table, which shows the y variable name and levels.
}
  \item{print.html}{
If TRUE, function prints a .html file to the current working directory.
}
  \item{html.filename}{
Character string indicating the name of the .html file that gets printed if print.html is set to TRUE.
}
}
\details{
NA
}
\value{
A character matrix with the requested frequency table. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
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
\code{\link{tabmeans}},
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

# Compare sex distribution by group, with group as column variable
freqtable1 <- tabfreq(x = d$Group, y = d$Sex)

# Same comparison, but compress table to show Female row only, show percent (SE) rather
# than n (percent), and suppress (n = ) from column headings
freqtable2 <- tabfreq(x = d$Group, y = d$Sex, compress = TRUE, compress.val = "Female",
                      cell = "col.percent", parenth = "se", n.headings = FALSE)

# Compare sex distribution by race, suppressing (n = ) from column headings and 
# showing percent (95\% CI) rather than n (percent)
freqtable3 <- tabfreq(x = d$Race, y = d$Sex, n.headings = FALSE, cell = "col.percent")

# Use rbind to create single table comparing sex and race in control vs. treatment group
freqtable4 <- rbind(tabfreq(x = d$Group, y = d$Sex), tabfreq(x = d$Group, y = d$Race))
                            
# A (usually) faster way to make the above table is to call the the tabmulti function
freqtable5 <- tabmulti(dataset = d, xvarname = "Group", yvarnames = c("Sex", "Race"))
                        
# freqtable4 and freqtable5 are equivalent
all(freqtable4 == freqtable5)

# Click on freqtable1, ... , freqtable5 in the Workspace tab of RStudio to see the tables 
# that could be copied and pasted into a report. Alternatively, setting the latex input to 
# TRUE produces tables that can be inserted into LaTeX using the xtable package.
}
\keyword{ table }
\keyword{ frequency }
\keyword{ crosstab }