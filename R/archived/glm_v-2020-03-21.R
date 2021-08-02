#' Call glm and Print a Summary Table to the RStudio Viewer
#'
#' You can call this function just as you would \code{\link[stats]{glm}}, but
#' the result is a summary table printed to the Viewer.
#'
#' @param ... Arguments to pass to glm.
#'
#' @return NULL
#'
#' @examples
#' glm_v(death_1yr ~ Age + Sex + Race, data = tabdata, family = "binomial")
#'
#' @export
glm_v <- function(...) {
  fit <- glm(...)
  print(tabglm(fit))
  return(NULL)
}
