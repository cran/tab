\name{tabcox}
\alias{tabcox}
\title{
Generate summary tables of Cox proportional hazards regression models for statistical reports. 
}
\description{
This function performs Cox proportional hazards regression using the R package survival [1] and summarizes the results in a clean table for a statistical report.
}
\usage{
tabcox(x, time, delta, xlabels=NULL, decimals=2, n=TRUE, events=TRUE, coef="n")
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
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses generic labels.
}
  \item{decimals}{
Number of decimal places for the regression coefficients, standard errors, hazard ratios, and confidence intervals. 
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
A character matrix with the results of the Cox PH regression. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. 
}
\references{
1. Therneau T (2013). A Package for Survival Analysis in S. R package version 2.37-4, \url{http://CRAN.R-project.org/package=survival}.

2. Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model.
Springer, New York. ISBN 0-387-98784-3.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{tabfreq}}
\code{\link{tabmeans}}
\code{\link{tablin}}
\code{\link{tablog}}
}
\examples{
# Load in example dataset d
data(d)

# Create labels for race
races = c("White","Black","Mexican American","Other")

# Test whether race is associated with survival
coxtable1 = tabcox(x=d$race,time=d$time,delta=d$delta,xlabels=c("Race",races))

# Test whether age, sex, race, and treatment group are associated with survival
coxtable2 = tabcox(x=d[,c("age","sex","race","group")],time=d$time,delta=d$delta,
                   xlabels=c("Age","Male","Race",races,"Treatment"))

# Click on coxtable1 and coxtable2 in the Workspace tab of RStudio to see the tables 
# that could be copied and pasted into a report or manuscript.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }
\keyword{ regression }
\keyword{ Cox proportional hazards}