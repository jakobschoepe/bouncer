#' @title Estimating smoothed model parameters
#' @description For objects of class \code{"bouncer"}, \code{coef()} estimates smoothed model parameters from approximated sampling distributions using bagging methodology.
#' @param object An object of class \code{"bouncer"}.
#' @details Smoothed estimates for model parameters are derived from 
#' \begin{equation*}
#' \tilde\beta^{*} = \frac{1}{k}\displaystyle\sum_{i=1}^{k} \hat\beta_{i}^{*}
#' \end{equation*}
#' where $k$ is the number of resampling replicates, and $\hat\beta_{i}^{*}$ is the estimate from the $i^{th}$ resampling replicate.
#' @return A numeric vector giving smoothed model parameter estimates.
#' @references Breiman L (1996) Bagging predictors. Mach Learn 24:123-140
#' @aliases coef,peims-method
#' @author Jakob Schöpe
#' @export

setMethod(f = "coef",
          signature = "bouncer",
          definition = function(object) {
            betaij <- object@betaij
            betaj <- colMeans(x = betaij, na.rm = TRUE)
            return(betaj)
          }
)
