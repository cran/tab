\name{tabglm}
\alias{tabglm}
\title{
Generate Summary Tables of Fitted Generalized Linear Models for Statistical Reports
}
\description{
This function takes an object returned from the glm function and generates a clean summary table for a statistical report.
}
\usage{
tabglm(glmfit, latex = FALSE, xlabels = NULL, ci.beta = TRUE, inference = "wald", 
       decimals = 2, p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, 
       p.leading0 = TRUE, p.avoid1 = FALSE, basic.form = FALSE, intercept = TRUE, 
       n = FALSE, events = FALSE, greek.beta = FALSE, binary.compress = TRUE, 
       bold.colnames = TRUE, bold.varnames = FALSE, bold.varlevels = FALSE, 
       predictor.colname = "Variable", print.html = FALSE, 
       html.filename = "table1.html")
}
\arguments{
  \item{glmfit}{
An object returned from glm function call.
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [1]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlabels}{
Optional character vector to label the x variables and their levels. If unspecified, the function uses the variable names and values themselves.
}
  \item{ci.beta}{
If TRUE, the table returned will include a column for 95\% confidence interval for the regression coefficients.
}
 \item{inference}{
If "wald", CI's and p-values are based on t or z statistics, depending on the GLM family (i.e. Gaussian, Poisson, binomial, etc.);
if "wald.z", CI's and p-values are based on z statistics; if "profile", CI's are based on profile likelihood (confint function), and p-values are based on t or z statistics, depending on the GLM family; if "profile.z", CI's are based on profile likelihood, and p-values are based on z statistics.
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
  \item{print.html}{
If TRUE, function prints a .html file to the current working directory.
}
  \item{html.filename}{
Character string indicating the name of the .html file that gets printed if print.html is set to TRUE.
}
}
\details{
The function should work well with categorical predictors (factors), provided they are not ordered. For ordered factors, just convert to unordered before creating the glm object to pass to tabglm. Note that you can define the levels of an unordered factor to control, which dictates which level is used as the reference group in regression models. For example, suppose a factor variable x takes values "low", "medium", and "high". If you write x = factor(x = x, levels = c("low", "medium", "high")), then you can run levels(x) to see that the levels are now arranged "low", "medium", "high". It is still a regular factor, but now if you use x as a predictor in a call to glm, "low" will be the reference group.

Interaction terms are compatible with tabglm, but the table will be formatted a little differently if interaction terms are present. Basically including an interaction is equivalent to setting basic.form to TRUE. All variable names and levels will be exactly as they appear when you run summary(glmfit), where glmfit is the object returned from a call to glm.
}
\value{
A character matrix that summarizes the fitted generalized linear model. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
}
\references{
1. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{https://cran.r-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
In older versions of RStudio, it was easier to copy tables from the Viewer and paste them directly into a text editor. The Viewer changed a few versions ago, and now it seems to work better if you paste into Microsoft Excel, and then copy again and paste into Microsoft Word. This is a little clumsy, so I recently added the new option to print a .html file with the table to your current working directory (see function inputs print.html and html.filename). Copying and pasting from the table from the .html file into a text editor seems to work well.

This function replaces the previous functions tablin and tablog, which were for linear and logistic regression, respectively. The only capability those functions had that tabglm does not currently have is the ability to calculate standardized regression coefficients. This feature may be added to tabglm in the future.

While tabglm should work with any object generated by a call to glm, not all possibilities have been tested. Therefore in general I recommend always doing a quick check that the table created by tabglm matches the information in the glm object itself.
}
\seealso{
\code{\link{glm}}
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmedians}},
\code{\link{tabmulti}},
\code{\link{tabcox}},
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

# Test whether age, sex, race, and treatment group are associated with BMI
glmfit1 <- glm(BMI ~ Age + Sex + Race + Group, data = d)
lintable <- tabglm(glmfit = glmfit1, 
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Test whether age, sex, race, and treatment group are associated with 1-year mortality
glmfit2 <- glm(death_1yr ~ Age + Sex + Race + Group, data = d, family = binomial)
logtable <- tabglm(glmfit = glmfit2, ci.beta = FALSE,
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Click on lintable or logtable in the Workspace tab of RStudio to see the tables that 
# could be copied and pasted into a report or manuscript. Alternatively, setting the
# latex input to TRUE produces tables that can be inserted into LaTeX using the xtable 
# package.
}
\keyword{ table }
\keyword{ generalized linear model }