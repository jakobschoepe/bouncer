#' @title  
#' @description \code{mpeims} is a wrapper function to approximate sampling distributions of model parameters for multiple data sets using \code{peims}.
#' @usage mpeims(X, data, ...)
#' @param X
#' @param data
#' @details
#' @return
#' @references
#' @author Jakob Schöpe
#' @examples
#' @export

mpeims <- function(X, data, ...) {
  # Check passed arguments to smoothly run subsequent commands
  if (!is.integer(x = X)) {
    stop("\"X\" must be a positive integer")
  }
  
  else if (!is.list(x = data)) {
    stop("\"data\" must be a list")
  }
  
  else {
    # Iterate dg() to
    tmp <- lapply(X = 1:X, FUN = function(i) {
      peims(data = data[[i]], ...)
    })
    return(tmp)
  }  
}
