#' @title Resampling methods for \code{peims}.
#' @description \code{resample} is a helper function for \code{peims} to resample from the original sample and subsequently fit the user-defined model.
#' @usage resample(i, data, size, replace, seed)
#' @param i Index of the replication (can be ignored). 
#' @param data A data frame or data table containing the variables in the model.
#' @param size A positive integer giving the number of observations to draw from the original sample.
#' @param replace A logical constant indicating if resampling should be with replacement.
#' @param seed An optional integer vector giving the state of Pierre L'Ecuyer's pseudo-random number generator for reproducibility purposes.
#' @details \code{resample} is primarily utilized as a helper function in \code{peims}, but is also suitable for reproducibility purposes.
#' @return A list containing the following elements:
#' \item{seed}{An integer vector giving the state of Pierre L'Ecuyer's pseudo-random number generator that was used to resample the original sample.}
#' \item{oir}{An integer vector giving the indices of resampled observations.}
#' \item{betaij}{A real vector giving the estimated model parameters.}
#' @references Work in progress.
#' @author Jakob Schöpe
#' @export

resample <- function(i, data, size, replace, seed) {
  # If the argument 'seed' is provided, set the kind and the state of the pseudo-random number generator.
  if (!missing(x = seed)) {
    if (is.integer(x = seed) & length(x = seed) == 7L) {
      RNGkind(kind = "L'Ecuyer-CMRG")
      .Random.seed <<- seed
    }
    
    else {
      stop("\"seed\" has been misspecified")
    }
  }
  
  else if (!exists(x = ".Random.seed")) {
    stop("state for the pseudo-random number generator has not been set")
  }
  
  else {  
    # Store the current state of the pseudo-random number generator for reproducability.
    seed <- .Random.seed
  
    # Resample from the original sample.
    oir <- sample(x = 1:nrow(x = data), size = size, replace = replace)
    data_tmp <- data[oir, ]

    # Fit the user-defined model to the resampled data set. 
    betaij <- f(data = data_tmp)
  
    return(list(seed = seed, oir = oir, betaij = betaij))
  }
}
