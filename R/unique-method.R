#' @title Summarizing unique models
#' @description For objects of class \code{"peims"}, \code{unique} summarizes unique models obtained from resampling.
#' @param x An object of class \code{"peims"}.
#' @return A data table
#' @references Work in progress.
#' @examples
#' f <- function(data) {
#' null <- glm(formula = y ~ 1, family = binomial, data = data)
#' full <- glm(formula = y ~ ., family = binomial, data = data)
#' fit <- coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
#' return(fit)
#' }
#'
#' fit <- peims(f = f, data = data, size = 100L, replace = TRUE, k = 5000L, seed = 123L, ncpus = 2L)
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
