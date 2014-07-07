\name{tab-package}
\alias{tab-package}
\alias{tab}
\docType{package}
\title{
Functions for creating summary tables for statistical reports
}
\description{
This package contains functions for generating tables for statistical reports written in Microsoft Word or LaTeX. There are functions for I-by-J frequency tables, comparison of means across levels of a categorical variable, generalized linear models, generalized estimating equations, and Cox proportional hazards regression. Functions are available to handle data simple random samples or survey data. The package is intended to make it easier for researchers to translate results from statistical analyses in R to their reports or manuscripts.
}
\details{
\tabular{ll}{
Package: \tab tab\cr
Type: \tab Package\cr
Version: \tab 2.1.2\cr
Date: \tab 2014-07-07\cr
License: \tab GPL-2\cr
}
The following functions are included in the package:

\code{\link{tabfreq}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans}},
\code{\link{tabmeans.svy}},
\code{\link{tabglm}},
\code{\link{tabglm.svy}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{formatp}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Therneau T (2013). A Package for Survival Analysis in S. R package version 2.37-4, \url{http://CRAN.R-project.org/package=survival}.

2. Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model.
Springer, New York. ISBN 0-387-98784-3.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Create labels for treatment group, sex, and race
groups <- c("Control", "Treatment")
sexes <- c("Female", "Male")
races <- c("White", "Black", "Mexican American", "Other")

# Compare race distribution by group, with group as column variable
freqtable <- tabfreq(x = d$group, y = d$race, xlevels = groups, 
                     ylevels = races, yname = "Race")

# Compare mean BMI in control group vs. treatment group
meanstable <- tabmeans(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI")

# Create a typical Table 1 for statistical report or manuscript
table1 <- rbind(tabmeans(x = d$group, y = d$age, xlevels = groups, yname = "Age"),
                tabmeans(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI"),
                tabfreq(x = d$group, y = d$sex, xlevels = groups, ylevels = sexes, 
                        yname = "Sex"),
                tabfreq(x = d$group, y = d$race, xlevels = groups, ylevels = races,
                        yname = "Race"))

# Test whether age, sex, race, and treatment group are associated with BMI
glmfit1 <- glm(bmi ~ age + sex + race + group, data = d)
lintable <- tabglm(glmfit = glmfit1, 
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Test whether age, sex, race, and treatment group are associated with 1-year mortality
glmfit2 <- glm(death_1yr ~ age + sex + race + group, data = d, family = binomial)
logtable <- tabglm(glmfit = glmfit2, ci.beta = FALSE,
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Test whether age, sex, race, and treatment group are associated with survival
coxtable <- tabcox(x = d[,c("age", "sex", "race", "group")], time = d$time, 
                   delta = d$delta, 
                   xlabels = c("Age", "Male", "Race", races, "Treatment"))

# Click on freqtable, meanstable, table1, lintable, logtable, and coxtable in 
# the Workspace tab of RStudio to see the tables that could be copied and pasted 
# into a Word document. Alternatively, setting the latex input to TRUE produces
# tables that can be inserted into LaTeX using the xtable package.
}