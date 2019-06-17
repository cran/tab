## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  message = FALSE
)

# Load packages
library("tab")
library("knitr")
library("gee")

# Set xtable options
options("xtable.caption.placement" = "top", 
        "xtable.include.rownames" = TRUE, 
        "xtable.comment" = FALSE)

## ------------------------------------------------------------------------
tabmulti(Age + Sex + Race ~ Group, data = tabdata) %>% kable()

## ------------------------------------------------------------------------
tabmulti(Age + Sex + Race ~ Group, data = tabdata, 
         yvarlabels = list(Age = "Age (years)", Race = "Race/ethnicity"), 
         ymeasures = c("median", "freq", "freq"), 
         listwise.deletion = TRUE, 
         n.headings = TRUE) %>% kable()

## ------------------------------------------------------------------------
fit <- glm(death_1yr ~ Age + Sex + Group, data = tabdata, family = binomial)
fit %>% tabglm(factor.compression = 5) %>% kable()

## ----echo = TRUE, eval = FALSE-------------------------------------------
#  tabdata2 <- reshape(data = tabdata,
#                      varying = c("bp.1", "bp.2", "bp.3", "highbp.1", "highbp.2", "highbp.3"),
#                      timevar = "bp.visit", direction = "long")
#  tabdata2 <- tabdata2[order(tabdata2$id), ]
#  fit <- gee(highbp ~ poly(Age, 2, raw = TRUE) + Sex + Race + Race*Sex,
#             id = id, data = tabdata2, family = "binomial", corstr = "unstructured")
#  fit %>% tabgee(data = tabdata2) %>% kable()

## ----include = FALSE-----------------------------------------------------
tabdata2 <- reshape(data = tabdata,
                    varying = c("bp.1", "bp.2", "bp.3", "highbp.1", "highbp.2", "highbp.3"),
                    timevar = "bp.visit", direction = "long")
tabdata2 <- tabdata2[order(tabdata2$id), ]
fit <- gee(highbp ~ poly(Age, 2, raw = TRUE) + Sex + Race + Race*Sex,
           id = id, data = tabdata2, family = "binomial", corstr = "unstructured")

## ----echo = FALSE--------------------------------------------------------
fit %>% tabgee(data = tabdata2) %>% kable()

## ------------------------------------------------------------------------
library("survival")
fit <- coxph(Surv(time = time, event = delta) ~ Age + Sex + Group, data = tabdata)
fit %>% tabcoxph(factor.compression = 5, columns = c("beta", "hr.ci")) %>% kable()

## ----message=FALSE, warning=FALSE----------------------------------------
library("survey")
design <- svydesign(
  data = tabsvydata,
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  nest = TRUE
)
tabmulti.svy(Age + Race + BMI ~ Sex, design = design) %>% kable()

## ----message=FALSE, warning=FALSE----------------------------------------
fit <- svyglm(BMI ~ Age + Sex + Race, design = design)
fit %>% tabglm(factor.compression = 3) %>% kable()

