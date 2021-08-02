## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  message = FALSE, 
  fig.width = 3
)

# Load packages
library("tab")
library("knitr")
library("gee")

# Set xtable options
options("xtable.caption.placement" = "top", 
        "xtable.include.rownames" = TRUE, 
        "xtable.comment" = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("vandomed/tab")
#  library("tab")

## ----eval = FALSE-------------------------------------------------------------
#  glm_v(
#    death_1yr ~ poly(Age, 2, raw = TRUE) + Sex * BMI,
#    data = tabdata,
#    family = binomial
#  )

## ----out.width = "100%", echo = FALSE-----------------------------------------
include_graphics("logistic.PNG")

## ----eval = FALSE-------------------------------------------------------------
#  tabmulti(Age + Sex + Race + BMI ~ Group, data = tabdata)

## ----out.width = "100%", echo = FALSE-----------------------------------------
include_graphics("tabmulti.PNG")

