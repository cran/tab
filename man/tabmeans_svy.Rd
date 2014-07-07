\name{tabmeans.svy}
\alias{tabmeans.svy}
\title{
Generate summary tables of mean comparisons for statistical reports (survey data)
}
\description{
This function compares the mean of a continuous variable across levels of a factor variable and summarizes the results in a clean table for a statistical report. Similar to tabmeans, but for survey data. Relies heavily on the 'survey' package [1,2].
}
\usage{
tabmeans.svy(x, y, svy, latex = FALSE, xlevels = NULL, yname = "Y variable", 
             test = "Wald", decimals = 1, p.decimals = c(2, 3), p.cuts = 0.01, 
             p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE)
}
\arguments{
  \item{svy}{
Survey design object created by a call to svydesign [1,2]. 
}
  \item{x}{
Character string specifying factor variable name. Must match one of names(svy$variables).
}
  \item{y}{
Character string specifying continuous variable name. Must match one of names(svy$variables).
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [3]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Optional label for the continuous variable.
}
  \item{test}{
Either "Wald" for Wald test or "LRT" for likelihood ratio test to test for equivalent mean y across levels of x. 
}
  \item{decimals}{
Number of decimal places for means and standard deviations or standard errors.
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
  \item{n}{
If TRUE, the table returned will include sample sizes in the column headings. 
}
}
\details{
NA
}
\value{
A character matrix with the requested table comparing mean y across levels of x. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [3].
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
\code{\link{tabfreq}}
\code{\link{tabfreq.svy}}
\code{\link{tabmeans}}
\code{\link{tabglm}}
\code{\link{tabglm.svy}}
\code{\link{tabcox}}
\code{\link{tabgee}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ means }
\keyword{ anova }
\keyword{ survey }