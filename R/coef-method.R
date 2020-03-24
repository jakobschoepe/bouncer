#' @title Estimating smoothed model parameters
#' @description For objects of class \code{"peims"}, \code{coef()} estimates smoothed model parameters from approximated sampling distributions using bagging methodology.
#' @param object An object of class \code{"peims"}.
#' @details Smoothed estimates for model parameters are derived from 
#' \begin{equation*}
#' \tilde\beta^{*} = \frac{1}{k}\displaystyle\sum_{i=1}^{k} \hat\beta_{i}^{*}
#' \end{equation*}
#' where $k$ is the number of resampling replicates, and $\hat\beta_{i}^{*}$ is the estimate from the $i^{th}$ resampling replicate.
#' @return A numeric vector giving smoothed model parameter estimates.
#' @references Breiman L (1996) Bagging predictors. Mach Learn 24:123-140
#' @aliases coef,peims-method
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
