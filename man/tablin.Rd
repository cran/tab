\name{tablin}
\alias{tablin}
\title{
Generate summary tables of linear regression models for statistical reports.
}
\description{
This function performs linear regression using the lm function and summarizes the results in a clean table for a statistical report.
}
\usage{
tablin(x, y, xlabels=NULL, int=TRUE, decimals=2, n=TRUE, coef="n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
For single predictor, vector of values; for multiple predictors, data frame or matrix with one column per predictor. Categorical variables should be of class "factor."
}
  \item{y}{
Vector of values for the response variable.
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
  \item{coef}{
If set to "x", function will standardize all variables in x that are continuous, providing standardized regression coefficients. Then the interpretation of each beta is the expected change in y for one standard deviation increase in the predictor. 

If set to "xy", the function will standardize all variables in x that are continuous and also standardize y, providing fully standardized regression coefficients. Then, the interpretation of each beta is the expected number of standard deviations that y changes for one standard deviation increase in the predictor.
}
}
\details{
NA
}
\value{
A character matrix with the results of the linear regression. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. 
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
\code{\link{tablog}}
\code{\link{tabcox}}
}
\examples{
# Load in example dataset d
data(d)

# Create labels for race
races = c("White","Black","Mexican American","Other")

# Test whether race is associated with BMI
lintable1 = tablin(x=d$race,y=d$bmi,xlabels=c("Race",races))

# Test whether age, sex, race, and treatment group are associated with BMI
lintable2 = tablin(x=d[,c("age","sex","race","group")],y=d$bmi,
                   xlabels=c("Age","Male","Race",races,"Treatment"))

# Same as previous, but request fully standardized regression coefficients
lintable3 = tablin(x=d[,c("age","sex","race","group")],y=d$bmi,
                   xlabels=c("Age","Male","Race",races,"Treatment"),coef="xy")

# Click on lintable1, lintable2, and lintable3 in the Workspace tab of RStudio 
# to see the tables that could be copied and pasted into a report or manuscript.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }
\keyword{ regression }