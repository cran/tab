\name{tabgee}
\alias{tabgee}
\title{
Generate summary tables of generalized estimating equations for statistical reports
}
\description{
This function takes an object returned from the gee function in the package gee [1] and generates a clean summary table for a statistical report.
}
\usage{
tabgee(geefit, latex = FALSE, xlabels = NULL, ci.beta = TRUE, decimals = 2, 
       p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
       p.avoid1 = FALSE, basic.form = FALSE, intercept = TRUE, n.id = FALSE, 
       n.total = FALSE, or = TRUE, robust = TRUE, data = NULL)
}
\arguments{
  \item{geefit}{
An object returned from gee function call.
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [2]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses the variable names and values themselves.
}
  \item{ci.beta}{
If TRUE, the table returned will include a column for Wald 95\% confidence interval for the estimated coefficients.
}
  \item{decimals}{
Number of decimal places for numeric values in the table (except p-values).
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
  \item{basic.form}{
If TRUE, there is no attempt to neatly format factor variables and their levels, and the table returned is very similar to what you see when you run summary(glmfit). 
}
  \item{intercept}{
If FALSE, the table returned will not include a row for the intercept.
}
  \item{n.id}{
If TRUE, the table returned will include a column for number of unique IDs (e.g. clusters).
}
  \item{n.total}{
If TRUE, the table returned will include a column for total number of observations used.
  }
  \item{or}{
If TRUE, the table returned will include columns for odds ratios and Wald 95\% confidence intervals for odds ratios. Only meaningful for logistic regression.
}
  \item{robust}{
If TRUE, robust standard errors are used (i.e. from sandwich estimator); if FALSE, naive standard errors are used.
}
  \item{data}{
Data frame or matrix containing variables passed to gee to create geefit. Only necessary when one or more of the predictors is a factor variable and basic.form is FALSE.
}
}
\details{
The function should work well with categorical predictors (factors), provided they are not ordered. For ordered factors, just convert to unordered before creating the gee object to pass to tabgee. Note that you can define the levels of an unordered factor to control, which dictates which level is used as the reference group in regression models. For example, suppose a factor variable x takes values "low", "medium", and "high". If you write x = factor(x = x, levels = c("low", "medium", "high")), then you can run levels(x) to see that the levels are now arranged "low", "medium", "high". It is still a regular factor, but now if you use x as a predictor in a call to gee, "low" will be the reference group when you call gee.

Interaction terms are compatible with tabgee, but the table will be formatted a little differently if interaction terms are present. Basically including an interaction is equivalent to setting basic.form to TRUE. All variable names and levels will be exactly as they appear when you run summary(geefit), where geefit is the object returned from a call to gee.
}
\value{
A character matrix that summarizes the fitted GEE. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [2].
}
\references{
1. Carey VJ (2012). gee: Generalized estimation equation solver. R package version 4.13-18. \url{http://CRAN.R-project.org/package=gee}.

2. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van domelen
}
\note{
While tabgee should work with any object generated by a call to gee, not all possibilities have been tested. Therefore in general I recommend always doing a quick check that the table created by tabgee matches the information in the gee object itself.
}
\seealso{
\code{\link{gee}}
\code{\link{tabfreq}}
\code{\link{tabfreq.svy}}
\code{\link{tabmeans}}
\code{\link{tabmeans.svy}}
\code{\link{tabglm}}
\code{\link{tabglm.svy}}
\code{\link{tabcox}}
}
\examples{
# Load in sample dataset d and convert to long format
data(d)
d2 <- reshape(data = d, 
              varying = c("bp.1", "bp.2", "bp.3", "highbp.1", "highbp.2", "highbp.3"), 
              timevar = "bp.visit", direction = "long")
d2 <- d2[order(d2$id), ]

# Load required package gee
library("gee")

# Create labels for race levels
races <- c("White", "Black", "Mexican American", "Other")

# Test whether predictors are associated with blood pressure at 1, 2, and 3 months
geefit1 <- gee(bp ~ age + sex + race + bmi + group, id = id, data = d2, 
               corstr = "unstructured")
               
# Create summary table using tabgee
geetable1 <- tabgee(geefit = geefit1, data = d2, n.id = TRUE, n.total = TRUE,
                    xlabels = c("Intercept", "Age", "Male", "Race", races, "BMI", 
                               "Treatment"))

# Test whether predictors are associated with high blood pressure at 1, 2, and 3 months
geefit2 <- gee(highbp ~ age + sex + race + bmi + group, id = id, data = d2, 
               family = binomial, corstr = "unstructured")
               
# Create summary table using tabgee
geetable2 <- tabgee(geefit = geefit2, data = d2, ci.beta = FALSE,
                    xlabels = c("Intercept", "Age", "Male", "Race", races, "BMI", 
                               "Treatment"))

# Click on geetable1 and geetable2 in the Workspace tab of RStudio to see the tables that 
# could be copied and pasted into a report or manuscript. Alternatively, setting the
# latex input to TRUE produces tables that can be inserted into LaTeX using the xtable 
# package.
}
\keyword{ table }
\keyword{ generalized estimating equation }
\keyword{ marginal model}