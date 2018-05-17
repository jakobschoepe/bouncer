#' @title Estimating smoothed model parameters
#' @description For objects of class \code{"peims"}, \code{coef} estimates smoothed model parameters from approximated sampling distributions using bagging methodology.
#' @param object An object of class \code{"peims"}.
#' @return A numeric vector giving smoothed model parameters.
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
