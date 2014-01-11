\name{tabmeans}
\alias{tabmeans}
\title{
Generate summary tables of mean comparisons for statistical reports. 
}
\description{
This function compares the mean of a continuous variable across levels of a factor variable and summarizes the results in a clean table for a statistical report.
}
\usage{
tabmeans(x, y, xlevels=NULL, yname="Y variable", decimals=1, n=TRUE, se=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector of values for the factor variable.
}
  \item{y}{
Vector of values for the continuous variable.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses generic labels.
}
  \item{yname}{
Optional label for the continuous varaible.
}
  \item{decimals}{
Number of decimal places for means and standard deviations or standard errors.
}
  \item{n}{
If TRUE, the table returned will include sample sizes in the column headings. 
}
  \item{se}{
If TRUE, the table will present mean (standard error) rather than mean (standard deviation).
}
}
\details{
If x has two levels, a t-test is used to test for a difference in means. An F test is first used to determine whether the equal variance or unequal variance t-test is appropriate. If x has more than two levels, a one-way analysis of variance is used to test for a difference in means across the groups.
}
\value{
A character matrix with the requested frequency table. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript.
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
\code{\link{tablin}}
\code{\link{tablog}}
\code{\link{tabcox}}
}
\examples{
# Load in example dataset d
data(d)

# Create labels for group and race
groups = c("Control","Treatment")
races = c("White","Black","Mexican American","Other")

# Compare mean BMI in control group vs. treatment group
meanstable1 = tabmeans(x=d$group,y=d$bmi,xlevels=groups,yname="BMI")

# Compare mean BMI by race and omit sample size
meanstable2 = tabmeans(x=d$race,y=d$bmi,xlevels=races,yname="BMI",n=FALSE)

# Create single table comparing mean BMI and mean age in control vs. treatment group
meanstable3 = rbind(tabmeans(x=d$group,y=d$bmi,xlevels=groups,yname="BMI"),
                    tabmeans(x=d$group,y=d$age,xlevels=groups,yname="Age"))

# Click on meanstable1, meanstable2, and meanstable3 in the Workspace tab of RStudio 
# to see the tables that could be copied and pasted into a report or manuscript.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }