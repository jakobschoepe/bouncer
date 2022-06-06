#' @title Summarizing unique models
#' @description For objects of class \code{"bouncer"}, \code{unique} summarizes unique models obtained from resampling.
#' @param x An object of class \code{"bouncer"}.
#' @return A data table
#' @export

setMethod(f = "unique",
          signature = "bouncer",
          definition = function(x) {
            betaij <- data.table::as.data.table(x = !is.na(x = x@betaij))
            betaij <- betaij[, .N, by = names(betaij)][order(N, decreasing = TRUE)]

            return(betaij)
          }
)
