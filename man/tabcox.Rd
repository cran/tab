\name{tabcox}
\alias{tabcox}
\title{
Generate Summary Tables of Fitted Cox Proportional Hazards Regression Models for Statistical Reports
}
\description{
This function performs Cox proportional hazards regression using the R package survival [1, 2] and summarizes the results in a clean table for a statistical report.
}
\usage{
tabcox(x, time, delta, latex = FALSE, xlabels = NULL, cluster = NULL, robust.se = TRUE, 
       decimals = 2, p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, 
       p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE, events = FALSE, coef = "n", 
       greek.beta = FALSE, binary.compress = TRUE, bold.colnames = TRUE, 
       bold.varnames = FALSE, bold.varlevels = FALSE, predictor.colname = "Variable", 
       suppress.beta = FALSE, print.html = FALSE, html.filename = "table1.html")
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
If TRUE, object returned is formatted for printing in LaTeX using xtable [3]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses generic labels.
}
  \item{cluster}{
Optional vector indicating clusters of subjects.
}
  \item{robust.se}{
Only a valid option if clusters are specified. In that case, setting to TRUE requests robust sandwich method standard errors, while setting to FALSE requests normal standard errors.
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
  \item{greek.beta}{
If TRUE, column headings refer to regression parameters as Greek letter beta rather than Beta. Only used when latex input is set to TRUE.
}
  \item{binary.compress}{
If TRUE, only one row of the table is dedicated to parameter estimates for each binary factor predictor. If FALSE, the table displays separate rows for the variable name and the two levels for each binary factor predictor, much like the presentation for factor variables with more than two levels.
}
  \item{bold.colnames}{
If TRUE, column headings are printed in bold font. Only applies if latex = TRUE. 
}
  \item{bold.varnames}{
If TRUE, variable names in the first column of the table are printed in bold font. Only applies if latex = TRUE.
}
  \item{bold.varlevels}{
If TRUE, levels of each factor variable are printed in bold font. Only applies if latex = TRUE and there is at least one factor variable included as a predictor.
}
  \item{predictor.colname}{
Character string with desired column heading for the column of predictors.
}
\item{suppress.beta}{
If FALSE, the Beta (SE) column is not included. May often be preferred since the point and interval estimate for the hazard ratio contains the same information, but are easier to interpret.
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
A character matrix with the results of the Cox PH regression. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [3].
}
\references{
1. Therneau T (2013). A Package for Survival Analysis in S. R package version 2.37-4, \url{https://cran.r-project.org/package=survival}.

2. Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model.
Springer, New York. ISBN 0-387-98784-3.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{https://cran.r-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
In older versions of RStudio, it was easier to copy tables from the Viewer and paste them directly into a text editor. The Viewer changed a few versions ago, and now it seems to work better if you paste into Microsoft Excel, and then copy again and paste into Microsoft Word. This is a little clumsy, so I recently added the new option to print a .html file with the table to your current working directory (see function inputs print.html and html.filename). Copying and pasting from the table from the .html file into a text editor seems to work well.

A function is currently being developed to create a summary table based on an object from coxph rather than the data vectors themselves (similarly to how tabglm works).

If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{coxph}},
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmedians}},
\code{\link{tabmulti}},
\code{\link{tabglm}},
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

# Create labels for race levels
races <- c("White", "Black", "Mexican American", "Other")

# Test whether race is associated with survival
coxtable1 <- tabcox(x = d$Race, time = d$time, delta = d$delta, 
                    xlabels = c("Race", races))

# Test whether age, sex, race, and treatment group are associated with survival
coxtable2 <- tabcox(x = d[,c("Age", "Sex", "Race", "Group")], time = d$time, 
                    delta = d$delta, 
                    xlabels = c("Age", "Male", "Race", races, "Treatment"))

# Click on coxtable1 or coxtable2 in the Workspace tab of RStudio to see the tables 
# that could be copied and pasted into a report or manuscript. Alternatively, setting 
# the latex input to TRUE produces tables that can be inserted into LaTeX using the 
# xtable package.
}
\keyword{ table }
\keyword{ regression }
\keyword{ Cox proportional hazards}