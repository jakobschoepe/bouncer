#' @title Summarizing unique models
#' @description For objects of class \code{"peims"}, \code{unique} summarizes unique models obtained from resampling.
#' @param x An object of class \code{"peims"}.
#' @return A data table
#' @references Work in progress.
#' @examples
#' f <- function(i, data, size, replace) {
#' data <- data[sample(x = 1:nrow(x = data), size = size, replace = replace),]
#' null <- glm(formula = mpg ~ 1, family = gaussian, data = data)
#' full <- glm(formula = mpg ~ . -id, family = gaussian, data = data)
#' fit <- coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
#' return(list(oir = data$id, betaij = fit))
#' }
#'
#' fit <- peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 10L, seed = 123L, ncpus = 2L)
#'
#' unique(fit)
#' @export

setMethod(f = "unique",
          signature = "peims",
          definition = function(x) {
            betaij <- data.table::as.data.table(x = !is.na(x = x@betaij))
            betaij <- betaij[, .N, by = names(betaij)][order(N, decreasing = TRUE)]

            return(betaij)
          }
)
