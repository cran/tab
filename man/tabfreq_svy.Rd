\name{tabfreq.svy}
\alias{tabfreq.svy}
\title{
Generate frequency tables for statistical reports (survey data)
}
\description{
This function creates an I-by-J frequency table and summarizes the results in a clean table for a statistical report. Similar to tabfreq, but for survey data. Relies heavily on the 'survey' package [1,2].
}
\usage{
tabfreq.svy(x, y, svy, latex = FALSE, xlevels = NULL, ylevels = NULL, 
            yname = "Y variable", test = "F", decimals = 1, p.decimals = c(2, 3), 
            p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
            p.avoid1 = FALSE, n = FALSE, compress = FALSE)
}
\arguments{
  \item{svy}{
Survey design object created by a call to svydesign [1,2]. 
}
  \item{x}{
Character string specifying column variable name. Must match one of names(svy$variables).
}
  \item{y}{
Character string specifying row variable name. Must match one of names(svy$variables).
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [3]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses the values that x takes on.
}
  \item{ylevels}{
Optional character vector to label the levels of y. If unspecified, the function uses the values that y takes on.
}
  \item{yname}{
Optional label for the y (row) variable.
}
  \item{test}{
Controls test for association between x and y. Must be a possible value for the 'statistic' input of the svychisq function in the survey package [1,2]: 'F', 'Chisq', 'Wald', 'adjWald', 'lincom', or 'saddlepoint'.
}
  \item{decimals}{
Number of decimal places for percentages.
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
  \item{n}{
If TRUE, the table returned will include sample sizes in the column headings. 
}
  \item{compress}{
If y has only two levels, setting compress to TRUE will produce a single row for n (percent) for the higher level. For example, if y is gender with 0 for female, 1 for male, setting compress = TRUE will return a table with n (percent) for males only.
}
}
\details{
NA
}
\value{
A character matrix with the requested frequency table. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [3].
}
\references{
1. Lumley T (2012). survey: analysis of complex survey samples. R package version 3.28-2, \url{http://CRAN.R-project.org/package=survey}.

2. Lumley T (2014). Analysis of complex survey samples. Journal of Statistical Software 9(1): 1-19.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Currently this function is fairly basic. Future versions should allow for more flexibility. If you have any specific suggestions for additional options, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{svydesign}}
\code{\link{svyglm}}
\code{\link{svychisq}}
\code{\link{tabfreq}}
\code{\link{tabmeans}}
\code{\link{tabmeans.svy}}
\code{\link{tabglm}}
\code{\link{tabglm.svy}}
\code{\link{tabcox}}
\code{\link{tabgee}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ frequency }
\keyword{ crosstab }
\keyword{ survey }