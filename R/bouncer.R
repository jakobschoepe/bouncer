#' @title Approximating sampling distributions of model parameters from model selection procedures
#' @description \code{bouncer} approximates sampling distributions of model parameters from model selection procedures using resampling methods.
#' @param f A user-defined function (see Details).
#' @param data A data frame containing the variables in the model.
#' @param size A positive integer giving the number of observations to draw from the original sample.
#' @param replace A logical constant indicating if resampling should be with replacement.
#' @param k A positive integer giving the number of resampling replicates.
#' @param seed An integer giving the random seed to initialize Pierre L'Ecuyer's multiple streams of pseudo-random numbers.
#' @param ncpus A positive integer giving the number of cores to be used during processing (\code{ncpus = 1} for serial processing is not recommended).
#' @param pkgs An optional character vector giving the names of the required packages.
#' @details The user-defined function passed to \code{f} should return a real vector containing the estimated model parameters and should have the following structure: \code{function(data) {Insert your code here.}}.
#' @return An object of S4 class \code{"bouncer"} containing the following slots:
#' \item{seedi}{A matrix containing the state of L'Ecuyer's pseudo-random number generator from each replication.}
#' \item{oir}{A matrix containing the frequency of draws per observation from each replication.}
#' \item{betaij}{A matrix containing the estimated model parameters from each replication.}
#' @author Jakob Schöpe
#' @export

bouncer <- function(f, data, size, replace, k, seed, ncpus, method, pkgs) {
  # Check passed arguments to smoothly run subsequent commands
  if (!is.function(x = f)) {
    stop("\"f\" must be a function")
  }

  else if (!is.element(el = "data", set = names(x = formals(fun = f))) && !is.element(el = "seed", set = names(x = formals(fun = f)))) {
    stop("\"f\" must contain the following arguments: \"data\" and \"seed\"")
  }
  
  else if (length(x = names(x = formals(fun = f))) != 2L) {
    stop("\"f\" should contain two arguments: \"data\" and \"seed\"")
  }

  else if (!is.data.frame(x = data)) {
    stop("\"data\" must be a data frame")
  }

  else if (!is.integer(x = size)) {
    stop("\"size\" must be a positive integer")
  }
  
  else if (length(x = size) != 1L) {
    stop("single positive integer for \"size\" expected")
  }
  
  else if (size < 1L) {
    stop("\"size\" must be a positive integer")
  }

  else if (size > nrow(x = data)) {
    stop("\"size\" exceeds the number of available observations in \"data\"")
  }

  else if (!is.logical(x = replace)) {
    stop("\"replace\" must be a logical value")
  }

  else if (length(x = replace) != 1L) {
    stop("single logical value for \"replace\" expected")
  }

  else if (!is.integer(x = k)) {
    stop("\"k\" must be a positive integer equal to or greater than 2")
  }
  
  else if (length(x = k) != 1L) {
    stop("single positive integer for \"k\" expected")
  }
  
  else if (k < 2L) {
    stop("\"k\" must be a positive integer equal to or greater than 2")
  }

  else if (isTRUE(x = replace) && k > choose(n = nrow(x = data) + size - 1, k = size)) {
    stop("\"size\" is to large considering ", k, " resampling replicates with replacement")
  }

  else if (!isTRUE(x = replace) && k > choose(n = nrow(x = data), k = size)) {
    stop("\"size\" is to large considering ", k, " resampling replicates without replacement")
  }

  else if (!is.integer(x = seed)) {
    stop("\"seed\" must be an integer")
  }

  else if (length(x = seed) != 1L) {
    stop("single integer for \"seed\" expected")
  }

  else if (!is.integer(x = ncpus)) {
    stop("\"ncpus\" must be a positive integer")
  }

  else if (length(x = ncpus) != 1L) {
    stop("single positive integer for \"ncpus\" expected")
  }
  
  else if (ncpus < 1L) {
    stop("\"ncpus\" must be a positive integer")
  }

  else if (ncpus > parallel::detectCores()) {
    stop("\"ncpus\" exceeds the number of detected cores")
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
  
  else {
    # Set up a cluster for parallel processing to speed up resampling and model fitting
    cluster <- parallel::makePSOCKcluster(names = ncpus, outfile = "log.txt")

    # Load required packages on each node to initialize parallel processing
    if (!missing(x = pkgs)) {
      if (!is.character(x = pkgs)) {
        parallel::stopCluster(cl = cluster)
        stop("\"pkgs\" must be a character vector")
      }
      else if (all(!(pkgs %in% rownames(x = installed.packages())))) {
        parallel::stopCluster(cl = cluster)
        stop("required package(s) not found in library")
      }
      else {
        parallel::clusterCall(cl = cluster, fun = lapply, X = pkgs, FUN = require, character.only = TRUE)
      }
    }

    # Export required objects to each node to initialize parallel processing
    parallel::clusterExport(cl = cluster, varlist = c('data', 'f', 'resample'), envir = environment())

    # Set seed for L'Ecuyer's pseudo-random number generator for reproducibility
    parallel::clusterSetRNGStream(cl = cluster, iseed = seed)

    # Run 'resample' on each initialized node 
    output <- pbapply::pblapply(cl = cluster, X = seq_len(k), FUN = resample, data = data, size = size, replace = replace, method = method)

    # Shut down the cluster
    parallel::stopCluster(cl = cluster)
    
    # Create a matrix that contains the state of L'Ecuyer's pseudo-random number generator from each replication to 
    # facilitate efficient reproducibility. 
    seedi <- as.matrix(x = data.table::rbindlist(l = lapply(X = seq_len(k), function(i) {as.list(x = output[[i]][["seed"]])})))

    # Create a matrix that contains the frequency of draws per observation from each replication to subsequently compute 
    # bootstrap covariances for confidence interval estimation (Note: NAs indicate zero frequency, but are transformed below!)
    oir <- as.matrix(x = data.table::rbindlist(l = lapply(X = seq_len(k), function(i) {as.list(x = table(output[[i]][["oir"]]))}), fill = TRUE))
    oir <- oir[, order(as.integer(x = colnames(x = oir)))]
    oir[is.na(x = oir)] <- 0

    # Create a matrix that contains the estimated model parameters from each replication to subsequently assess instability 
    # in model selection and to compute smoothed estimates through bagging with their corresponding confidence intervals
    betaij <- as.matrix(x = data.table::rbindlist(l = lapply(X = seq_len(k), function(i) {as.list(x = output[[i]][["betaij"]])}), fill = TRUE))
    betaij <- betaij[, order(colnames(x = betaij))]

    return(new(Class = "bouncer", seedi = seedi, oir = oir, betaij = betaij))
  }
}
