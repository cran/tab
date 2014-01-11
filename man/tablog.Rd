\name{tablog}
\alias{tablog}
\title{
Generate summary tables of logistic regression models for statistical reports.
}
\description{
This function performs logistic regression using the glm function and summarizes the results in a clean table for a statistical report.
}
\usage{
tablog(x, y, xlabels=NULL, int=TRUE, decimals=2, n=TRUE, events=TRUE, coef="n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
For single predictor, vector of values; for multiple predictors, data frame or matrix with one column per predictor. Categorical variables should be of class "factor."
}
  \item{y}{
Vector of 0's and 1's where 1's indicate event.
}
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses generic labels.
}
  \item{int}{
If FALSE, the table returned will not include a row for the intercept (the model is still fit with an intercept).
}
  \item{decimals}{
Number of decimal places for the regression coefficients, standard errors, and confidence intervals. 
}
  \item{n}{
If TRUE, the table returned will include a column for sample size. 
}
  \item{events}{
If TRUE, the table returned will include a column for number of events observed.
}
  \item{coef}{
If set to "x", function will standardize all variables in x that are continuous, providing standardized regression coefficients. Then the interpretation of each odds ratio is the odds ratio associated with a one standard deviation increase in the predictor.
}
}
\details{
NA
}
\value{
A character matrix with the results of the Cox PH regression. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript.
}
\references{
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
\code{\link{tabcox}}
}
\examples{
# Load in example dataset d
data(d)

# Create labels for race
races = c("White","Black","Mexican American","Other")

# Test whether race is associated with 1-year mortality
logtable1 = tablog(x=d$race,y=d$death_1yr,xlabels=c("Race",races))

# Test whether age, sex, race, and treatment group are associated with 1-year mortality
logtable2 = tablog(x=d[,c("age","sex","race","group")],y=d$death_1yr,
                   xlabels=c("Age","Male","Race",races,"Treatment"))

# Click on logtable1 and logtable2 in the Workspace tab of RStudio to see the 
# tables that could be copied and pasted into a report or manuscript.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }
\keyword{ logistic regression }