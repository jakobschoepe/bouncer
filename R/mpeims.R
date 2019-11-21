#' @title  
#' @description \code{mpeims} is a wrapper function to approximate sampling distributions of model parameters for multiple data sets using \code{peims}.
#' @usage mpeims(X, f, data, size, replace, k, seed, ncpus, pkgs)
#' @param X
#' @param f
#' @param data
#' @param size
#' @param replace
#' @param k
#' @param seed
#' @param ncpus
#' @param pkgs
#' @details
#' @return
#' @references
#' @author Jakob Schöpe
#' @examples
#' @export

mpeims <- function(f, data, size, replace, k, seed, ncpus, pkgs) {
  # Check passed arguments to smoothly run subsequent commands
  if (!is.list(x = data)) {
    stop("\"data\" must be a list")
  }
  
  else if (
  
  
  else if (length(x = seed) == length(x = data)) {
    stop(paste("\"seed\" must be of length", length(x = data)) 
  }
  
  else {
  
    tmp <- lapply(X = 1:length(x = data), function(i) {
      peims(f = f, data = data[[i]], size = size[[i]], replace = replace[[i]], k = k[[i]], seed = seed[[i]], ncpus = ncpus, pkgs = pkgs)
    })
  
    return(tmp)
  }
}
