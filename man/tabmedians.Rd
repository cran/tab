\name{tabmedians}
\alias{tabmedians}
\title{
Generate Summary Tables of Median Comparisons for Statistical Reports 
}
\description{
This function compares the median of a continuous variable across levels of a categorical variable and summarizes the results in a clean table for a statistical report.
}
\usage{
tabmedians(x, y, latex = FALSE, xlevels = NULL, yname = "Y variable", decimals = 1, 
           p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
           p.avoid1 = FALSE, n = FALSE, parenth = "iqr", text.label = NULL, 
           parenth.sep = "-")
}
\arguments{
  \item{x}{
Vector of values for the categorical variable.
}
  \item{y}{
Vector of values for the continuous variable.
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [1]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Optional label for the continuous variable.
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
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the medians in each cell. Possible choices are as follows: 'minmax' for minimum and maximum; 'range' for difference between minimum and maximum; 'q1q3' for first and third quartiles; 'iqr' for difference between first and third quartiles; or 'none' for no parentheses at all.
}
  \item{text.label}{
Optional text to put after the variable name. For example, if parenth is 'q1q3' and yname is 'BMI' the default label would be 'BMI, Median (Q1-Q3)'. You might prefer to set text.label to something like 'Med (Quartile 1-Quartile 3)' instead.
}
  \item{parenth.sep}{
Optional character specifying the separator for the two numbers in parentheses when parenth is set to 'minmax' or 'q1q3'. The default is a dash, so values in the table are formatted as Median (Lower-Upper). If you set parenth.sep to ', ' the values in the table would instead be formatted as Median (Lower, Upper).
}
}
\details{
If x has two levels, a Mann-Whitney U (also known as Wilcoxon rank-sum) test is used to test whether the distribution of the continuous variable (y) differs in the two groups (x). If x has more than two levels, a Kruskal-Wallis test is used to test whether the distribution of y differs across at least two of the x groups.

Both x and y can have missing values. The function drops observations with missing x or y. 
}
\value{
A character matrix with the requested table comparing median y across levels of x. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
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
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmulti}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans.svy}},
\code{\link{tabglm.svy}},
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Create labels for group and race
groups <- c("Control", "Treatment")
races <- c("White", "Black", "Mexican American", "Other")

# Compare median BMI in control group vs. treatment group
medtable1 <- tabmedians(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI")

# Repeat, but suppress Min-Max from being shown in parentheses
medtable2 <- tabmedians(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI", 
             parenth = "none")

# Compare median BMI by race and include sample size
medtable3 <- tabmedians(x = d$race, y = d$bmi, xlevels = races, yname = "BMI", n = TRUE)

# Create single table comparing median BMI and median age in control vs. treatment group
medtable4 <- rbind(tabmedians(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI"),
                   tabmedians(x = d$group, y = d$age, xlevels = groups, yname = "Age"))
                   
# An easier way to make the above table is to call the tabmulti function
medtable5 <- tabmulti(dataset = d, xvarname = "group", yvarnames = c("bmi", "age"),
                      ymeasures = "median", xlevels = groups, ynames = c("BMI", "Age"))
                        
# medtable4 and medtable5 are equivalent
all(medtable4 == medtable5)

# Click on medtable1, medtable2, medtable3, medtable4, or medtable5 in the Workspace tab 
# of RStudio to see the tables that could be copied and pasted into a report or 
# manuscript. Alternatively, setting the latex input to TRUE produces tables that can be 
# inserted into LaTeX using the xtable package.
}
\keyword{ table }
\keyword{ median }