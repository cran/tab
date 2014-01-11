\name{tabfreq}
\alias{tabfreq}
\title{
Generate frequency tables for statistical reports. 
}
\description{
This function creates an I-by-J frequency table and summarizes the results in a clean table for a statistical report.
}
\usage{
tabfreq(x, y, test="chi", xlevels=NULL, ylevels=NULL, yname="Y variable", 
        decimals=1, n=TRUE, compress=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector of values indicating group membership for columns of IxJ table. 
}
  \item{y}{
Vector of values indicating group membership for rows of IxJ table. 
}
  \item{test}{
Controls test of independence between x and y. "chi" for Pearson's chi-squared test, "fisher" for Fisher's exact test.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses generic labels.
}
  \item{ylevels}{
Optional character vector to label the levels of y. If unspecified, the function uses generic labels.
}
  \item{yname}{
Optional label for the y (row) variable.
}
  \item{decimals}{
Number of decimal places for percentages.
}
  \item{n}{
If TRUE, the table returned will include sample sizes in the column headings. 
}
  \item{compress}{
If y has only two levels, setting compress to TRUE will produce a single row for n (percent) for the higher level. For example, if y is gender with 0 for female, 1 for male, setting compress=TRUE will return a table with n (percent) for males only.
}
}
\details{
NA
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
\code{\link{tabmeans}}
\code{\link{tablin}}
\code{\link{tablog}}
\code{\link{tabcox}}
}
\examples{
# Load in example dataset d
data(d)

# Create labels for treatment group, sex, and race
groups = c("Control","Treatment")
sexes = c("Female","Male")
races = c("White","Black","Mexican American","Other")

# Compare sex distribution by group, with group as column variable
freqtable1 = tabfreq(x=d$group,y=d$sex,xlevels=groups,ylevels=sexes,yname="Sex")

# Same comparison, but compress table to single row
freqtable2 = tabfreq(x=d$group,y=d$sex,xlevels=groups,yname="Male",compress=TRUE)

# Compare sex distribution by race, with race as column variable, and omit sample sizes
freqtable3 = tabfreq(x=d$race,y=d$sex,xlevels=races,yname="Sex",ylevels=sexes,n=FALSE)

# Use rbind to create single table comparing sex and race in control vs. treatment group
freqtable4 = rbind(tabfreq(x=d$group,y=d$sex,xlevels=groups,ylevels=sexes,yname="Sex"),
                   tabfreq(x=d$group,y=d$race,xlevels=groups,ylevels=races,yname="Race"))

# Click on freqtable1, freqtable2, freqtable3, and freqtable4 in the Workspace tab of 
# RStudio to see the tables that could be copied and pasted into a report or manuscript.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }
\keyword{ frequency }
\keyword{ crosstab }