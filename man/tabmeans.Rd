% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabmeans.R
\name{tabmeans}
\alias{tabmeans}
\title{Create Table Comparing Group Means}
\usage{
tabmeans(formula = NULL, data = NULL, x = NULL, y = NULL,
  columns = c("xgroups", "p"), parenth = "sd", sep.char = ", ",
  variance = "unequal", xlevels = NULL, yname = NULL,
  text.label = NULL, quantiles = NULL, quantile.vals = FALSE,
  decimals = NULL, formatp.list = NULL, n.headings = TRUE,
  print.html = FALSE, html.filename = "table1.html")
}
\arguments{
\item{formula}{Formula, e.g. \code{BMI ~ Group}.}

\item{data}{Data frame containing variables named in \code{formula}.}

\item{x}{Vector of values for the categorical \code{x} variable.}

\item{y}{Vector of values for the continuous \code{y} variable.}

\item{columns}{Character vector specifying what columns to include. Choices
for each element are \code{"n"} for total sample size, \code{"overall"} for
overall mean, \code{"xgroups"} for \code{x} group means, \code{"diff"} for
difference in \code{x} group means (this one and the next two are only
available for binary \code{x}), \code{"diffci"} for 95% CI for difference in
\code{x} group means, \code{"diff.ci"} for difference in group means and 95%
confidence interval, \code{"test"} for test statistic, and \code{"p"} for
p-value.}

\item{parenth}{Character string specifying what statistic to display in
parentheses after the means. Choices are \code{"none"}, \code{"sd"},
\code{"se"},  \code{"t.ci"}, \code{"z.ci"}, \code{"range"}, and
\code{"minmax"}.}

\item{sep.char}{Character string with separator to place between lower and
upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.}

\item{variance}{Character string specifying which version of the two-sample
t-test to use if \code{x} has 2 levels. Choices are \code{"equal"} for equal
variance t-test, \code{"unequal"} for unequal variance t-test, and \code{"f"}
for F test to determine which to use.}

\item{xlevels}{Character vector with labels for the levels of \code{x}, used
in column headings.}

\item{yname}{Character string with a label for the \code{y} variable.}

\item{text.label}{Character string with text to put after the \code{y}
variable name, identifying what cell values and parentheses represent.}

\item{quantiles}{Numeric value. If specified, table compares \code{y} across
quantiles of \code{x} created on the fly.}

\item{quantile.vals}{Logical value for whether labels for \code{x} quantiles
should show quantile number and corresponding range, e.g. Q1 [0.00, 0.25),
rather than just the quantile number.}

\item{decimals}{Numeric value specifying number of decimal places for numbers
other than p-values.}

\item{formatp.list}{List of arguments to pass to \code{\link[tab]{formatp}}.}

\item{n.headings}{Logical value for whether to display group sample sizes in
parentheses in column headings.}

\item{print.html}{Logical value for whether to write a .html file with the
table to the current working directory.}

\item{html.filename}{Character string specifying the name of the .html file
that gets written if \code{print.html = TRUE}.}
}
\value{
Data frame which you can print in R (e.g. with \strong{xtable}'s
\code{\link[xtable]{xtable}} or \strong{knitr}'s \code{\link[knitr]{kable}})
or export to Word, Excel, or some other program. To export the table, set
\code{print.html = TRUE}. This will result in a .html file being written to
your current working directory, which you can open and copy/paste into your
document.
}
\description{
Creates a table comparing the mean of \code{y} across levels of \code{x}.
}
\details{
A t-test is used to compare means if \code{x} has two levels, and a one-way
analysis of variance is used if \code{x} has more than two levels.
Observations with missing values for \code{x} and/or \code{y} are dropped.
}
\examples{
# Compare mean BMI in control vs. treatment group in sample dataset
(meanstable1 <- tabmeans(BMI ~ Group, data = tabdata))

# Same as previous, but specifying input vectors rather than formula
(meanstable2 <- tabmeans(x = tabdata$Group, y = tabdata$BMI))

# Compare mean baseline systolic BP across tertiles of BMI
(meanstable3 <- tabmeans(bp.1 ~ BMI, data = tabdata,
                         quantiles = 3, yname = "Systolic BP"))

# Create single table comparing mean BMI and mean age in control vs.
# treatment group. Drop missing observations first.
tabdata2 <- subset(tabdata, ! is.na(BMI) & ! is.na(Age))
(meanstable4 <- rbind(tabmeans(BMI ~ Group, data = tabdata2),
                      tabmeans(Age ~ Group, data = tabdata2)))

# Same as previous, but using tabmulti for convenience
(meanstable5 <- tabmulti(BMI + Age ~ Group, data = tabdata))


}
