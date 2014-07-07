\name{tabcox}
\alias{tabcox}
\title{
Generate summary tables of Cox proportional hazards regression models for statistical reports
}
\description{
This function performs Cox proportional hazards regression using the R package survival [1, 2] and summarizes the results in a clean table for a statistical report.
}
\usage{
tabcox(x, time, delta, latex = FALSE, xlabels = NULL, decimals = 2, p.decimals = c(2, 3), 
       p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = TRUE,
       events = TRUE, coef = "n")
}
\arguments{
  \item{x}{
For single predictor, vector of values; for multiple predictors, data frame or matrix with one column per predictor. Categorical variables should be of class "factor."
}
  \item{time}{
Numeric values for time to event or censoring.
}
  \item{delta}{
Indicator variable where 1 = event observed, 0 = censored.
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [3]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses generic labels.
}
  \item{decimals}{
Number of decimal places for the regression coefficients, standard errors, hazard ratios, and confidence intervals. 
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
If TRUE, the table returned will include a column for sample size. 
}
  \item{events}{
If TRUE, the table returned will include a column for number of events observed (i.e. uncensored observations).
}
  \item{coef}{
If set to "x", function will standardize all variables in x that are continuous, providing standardized regression coefficients. Then, the interpretation of each hazard ratio is the hazard ratio associated with a one standard deviation increase in the predictor.
}
}
\details{
NA
}
\value{
A character matrix with the results of the Cox PH regression. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [3].
}
\references{
1. Therneau T (2013). A Package for Survival Analysis in S. R package version 2.37-4, \url{http://CRAN.R-project.org/package=survival}.

2. Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model.
Springer, New York. ISBN 0-387-98784-3.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
A function is currently being developed to create a summary table based on an object from coxph rather than the data vectors themselves (similarly to how tabglm works).

If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{coxph}}
\code{\link{tabfreq}}
\code{\link{tabfreq.svy}}
\code{\link{tabmeans}}
\code{\link{tabmeans.svy}}
\code{\link{tabglm}}
\code{\link{tabglm.svy}}
\code{\link{tabgee}}
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Create labels for race levels
races <- c("White", "Black", "Mexican American", "Other")

# Test whether race is associated with survival
coxtable1 <- tabcox(x = d$race, time = d$time, delta = d$delta, 
                    xlabels = c("Race", races))

# Test whether age, sex, race, and treatment group are associated with survival
coxtable2 <- tabcox(x = d[,c("age", "sex", "race", "group")], time = d$time, 
                    delta = d$delta, 
                    xlabels = c("Age", "Male", "Race", races, "Treatment"))

# Click on coxtable1 and coxtable2 in the Workspace tab of RStudio to see the tables 
# that could be copied and pasted into a report or manuscript. Alternatively, setting 
# the latex input to TRUE produces tables that can be inserted into LaTeX using the 
# xtable package.
}
\keyword{ table }
\keyword{ regression }
\keyword{ Cox proportional hazards}