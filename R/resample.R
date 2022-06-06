#' @title Resampling methods for \code{bouncer}.
#' @description \code{resample} is a helper function for \code{bouncer} to resample from the original sample and subsequently fit the user-defined model.
#' @usage resample(i, data, size, replace, seed)
#' @param i Index of the replication (can be ignored). 
#' @param data A data frame or data table containing the variables in the model.
#' @param size A positive integer giving the number of observations to draw from the original sample.
#' @param replace A logical constant indicating if resampling should be with replacement.
#' @param seed An optional integer vector giving the state of Pierre L'Ecuyer's pseudo-random number generator for reproducibility purposes.
#' @param method
#' @details \code{resample} is primarily utilized as a helper function in \code{bouncer}, but is also suitable for reproducibility purposes.
#' @return A list containing the following elements:
#' \item{seed}{An integer vector giving the state of Pierre L'Ecuyer's pseudo-random number generator that was used to resample the original sample.}
#' \item{oir}{An integer vector giving the indices of resampled observations.}
#' \item{betaij}{A real vector giving the estimated model parameters.}
#' @author Jakob Schöpe
#' @export

resample <- function(i, data, size, replace, seed, method) {
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
  else if (!is.character(x = method)) {
    stop("\"method\" must be a character string")
  }
  
  else if (length(x = method) != 1L) {
    stop("single character string for \"method\" expected")
  }
  
  else if (!(method %in% c("simple", "block"))) {
    stop("\"method\" is misspecified")
  }
  
  else if (!exists(x = ".Random.seed")) {
    stop("state for the pseudo-random number generator has not been set")
  }
  
  else {  
    # Store the current state of the pseudo-random number generator for reproducability.
    seed <- .Random.seed
    if (method == "simple") {
      # Resample from the original sample.
      oir <- sample(x = seq_len(nrow(data)), size = size, replace = replace)
      data_tmp <- data[oir, ]
    }
    
    if (method == "block") {
      oir <- sample(x = unique(data$id), size = size, replace = replace)
      id_tmp <- sapply(seq_len(length(oir)), function(i) {which(data$id == oir[i])})
      data_tmp <- data[id_tmp, ]
      data_tmp$id <- rep(seq_len(ncol(id_tmp)), each = nrow(id_tmp))
    }
    
    # Fit the user-defined model to the resampled data set. 
    betaij <- f(data = data_tmp)
  
    return(list(seed = seed, oir = oir, betaij = betaij))
  }
}
