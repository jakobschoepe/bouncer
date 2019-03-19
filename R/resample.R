#' @title Resampling methods for \code{peims}.
#' @description \code{resample} resamples from the original sample and fits the user-defined model to the resampled data.
#' @param i Index of the repetition (can be ignored). 
#' @param data A data frame or data table containing the variables in the model.
#' @param size A positive integer giving the number of observations to draw from the original sample.
#' @param replace A logical constant indicating if resampling should be with replacement.
#' @param seed An integer giving the seed to initialize Pierre L'Ecuyer's pseudo-random number generator.
#' @details 
#' @references Work in progress.
#' @examples
#' @export

resample <- function(i, data, size, replace, seed) {
  # If the argument 'seed' is provided, set the kind and the state of the pseudo-random number generator.
  if (!missing(x = seed)) {
    RNGkind(kind = "L'Ecuyer-CMRG")
    .Random.seed <<- seed
  }
  
  # Store the current state of the pseudo-random number generator for reproducability.
  seed <- .Random.seed
  
  # Resample from the original sample.
  data_tmp <- data[sample(x = 1:nrow(x = data), size = size, replace = replace), ]

  oir <- data_tmp$tmp_id
  data_tmp$tmp_id <- NULL
  
  # Fit the user-defined model to the resampled data. 
  betaij <- f(data = data_tmp)
  
  return(list(seed = seed, oir = oir, betaij = betaij))
}