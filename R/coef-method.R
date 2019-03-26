#' @title Estimating smoothed model parameters
#' @description For objects of class \code{"peims"}, \code{coef} estimates smoothed model parameters from approximated sampling distributions using bagging methodology.
#' @param object An object of class \code{"peims"}.
#' @return A real vector giving estimated smoothed model parameters.
#' @references Work in progress.
#' @author Jakob Schöpe
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
#' coef(fit)
#' @export

setMethod(f = "coef",
          signature = "peims",
          definition = function(object) {
            betaij <- object@betaij
            betaj <- colMeans(x = betaij, na.rm = TRUE)
            return(betaj)
          }
)
