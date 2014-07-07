\name{tabfreq}
\alias{tabfreq}
\title{
Generate frequency tables for statistical reports
}
\description{
This function creates an I-by-J frequency table and summarizes the results in a clean table for a statistical report.
}
\usage{
tabfreq(x, y, latex = FALSE, xlevels = NULL, ylevels = NULL, yname = "Y variable", 
        test = "chi", decimals = 1, p.decimals = c(2, 3), p.cuts = 0.01, 
        p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE, 
        compress = FALSE)
}
\arguments{
  \item{x}{
Vector of values indicating group membership for columns of IxJ table. 
}
  \item{y}{
Vector of values indicating group membership for rows of IxJ table. 
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [1]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
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
Controls test for association between x and y. "chi" for Pearson's chi-squared test, "fisher" for Fisher's exact test, "z" for z test without continuity correction, and "z.continuity" for z test with continuity correction. "z" and "z.continuity" can only be used if x and y are binary.
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
A character matrix with the requested frequency table. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
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
\code{\link{svydesign}}
\code{\link{svyglm}}
\code{\link{tabfreq.svy}}
\code{\link{tabmeans}}
\code{\link{tabmeans.svy}}
\code{\link{tabglm}}
\code{\link{tabglm.svy}}
\code{\link{tabcox}}
\code{\link{tabgee}}
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Create labels for treatment group, sex, and race
groups <- c("Control", "Treatment")
sexes <- c("Female", "Male")
races <- c("White", "Black", "Mexican American", "Other")

# Compare sex distribution by group, with group as column variable
freqtable1 <- tabfreq(x = d$group, y = d$sex, xlevels = groups, ylevels = sexes, 
                      yname = "Sex")

# Same comparison, but compress table to single row and include sample size
freqtable2 <- tabfreq(x = d$group, y = d$sex, xlevels = groups, yname = "Male", 
                      compress = TRUE, n = TRUE)

# Compare sex distribution by race, with race as column variable
freqtable3 <- tabfreq(x = d$race, y = d$sex, xlevels = races, yname = "Sex", 
                      ylevels = sexes)

# Use rbind to create single table comparing sex and race in control vs. treatment group
freqtable4 <- rbind(tabfreq(x = d$group, y = d$sex, xlevels = groups, 
                            ylevels = sexes, yname = "Sex"),
                    tabfreq(x = d$group, y = d$race, xlevels = groups, 
                            ylevels = races, yname = "Race"))

# Click on freqtable1, freqtable2, freqtable3, and freqtable4 in the Workspace tab of 
# RStudio to see the tables that could be copied and pasted into a report or manuscript.
# Alternatively, setting the latex input to TRUE produces tables that can be inserted 
# into LaTeX using the xtable package.
}
\keyword{ table }
\keyword{ frequency }
\keyword{ crosstab }