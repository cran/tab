\name{tabglm}
\alias{tabglm}
\title{
Generate summary tables of generalized linear models for statistical reports
}
\description{
This function takes an object returned from the glm function and generates a clean summary table for a statistical report.
}
\usage{
tabglm(glmfit, latex = FALSE, xlabels = NULL, ci.beta = TRUE, decimals = 2, 
       p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
       p.avoid1 = FALSE, basic.form = FALSE, intercept = TRUE, n = FALSE, 
       events = FALSE, or = TRUE)
}
\arguments{
  \item{glmfit}{
An object returned from glm function call.
}
  \item{latex}{
If TRUE, object returned will be formatted for printing in LaTeX using xtable [1]; if FALSE, it will be formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses the variable names and values themselves.
}
  \item{ci.beta}{
If TRUE, the table returned will include a column for Wald 95\% confidence interval for the regression coefficients.
}
  \item{decimals}{
Number of decimal places for numeric values in the table (except p-values).
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
  \item{basic.form}{
If TRUE, there is no attempt to neatly format factor variables and their levels, and the table returned is very similar to what you see when you run summary(glmfit). 
}
  \item{intercept}{
If FALSE, the table returned will not include a row for the intercept.
}
  \item{n}{
If TRUE, the table returned will include a column for sample size.
}
  \item{events}{
If TRUE, the table returned will include a column for number of events observed. Only meaningful when the outcome variable is binary.
}
  \item{or}{
If TRUE, the table returned will include columns for odds ratios and Wald 95\% confidence intervals for odds ratios. Only meaningful for logistic regression.
}
}
\details{
The function should work well with categorical predictors (factors), provided they are not ordered. For ordered factors, just convert to unordered before creating the glm object to pass to tabglm. Note that you can define the levels of an unordered factor to control, which dictates which level is used as the reference group in regression models. For example, suppose a factor variable x takes values "low", "medium", and "high". If you write x = factor(x = x, levels = c("low", "medium", "high")), then you can run levels(x) to see that the levels are now arranged "low", "medium", "high". It is still a regular factor, but now if you use x as a predictor in a call to glm, "low" will be the reference group when you call glm.

Interaction terms are compatible with tabglm, but the table will be formatted a little differently if interaction terms are present. Basically including an interaction is equivalent to setting basic.form to TRUE. All variable names and levels will be exactly as they appear when you run summary(glmfit), where glmfit is the object returned from a call to glm.
}
\value{
A character matrix that summarizes the fitted generalized linear model. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
}
\references{
1. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van domelen
}
\note{
This function replaces the previous functions tablin and tablog, which were for linear and logistic regression, respectively. The only capability those functions had that tabglm does not currently have is the ability to calculate standardized regression coefficients. This feature may be added to tabglm in the future.

While tabglm should work with any object generated by a call to glm, not all possibilities have been tested. Therefore in general I recommend always doing a quick check that the table created by tabglm matches the information in the glm object itself.
}
\seealso{
\code{\link{glm}}
\code{\link{tabfreq}}
\code{\link{tabmeans}}
\code{\link{tabcox}}
\code{\link{tabgee}}
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d),]

# Create labels for race levels
races <- c("White", "Black", "Mexican American", "Other")

# Test whether age, sex, race, and treatment group are associated with BMI
glmfit1 <- glm(bmi ~ age + sex + race + group, data = d)
lintable <- tabglm(glmfit = glmfit1, 
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Test whether age, sex, race, and treatment group are associated with 1-year mortality
glmfit2 <- glm(death_1yr ~ age + sex + race + group, data = d, family = binomial)
logtable <- tabglm(glmfit = glmfit2, ci.beta = FALSE,
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Click on lintable and logtable in the Workspace tab of RStudio to see the tables that 
# could be copied and pasted into a report or manuscript. Alternatively, setting the
# latex input to TRUE produces tables that can be inserted into LaTeX using the xtable 
# package.
}
\keyword{ table }
\keyword{ generalized linear model }