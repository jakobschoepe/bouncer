#' @title S4 Class \code{"peims"}
#' @slot seedi A matrix containing the state of L'Ecuyer's pseudo-random number generator from each replication.
#' @slot oir A matrix containing the frequency of draws per observation from each replication.
#' @slot betaij A matrix containing the estimated model parameters from each replication.
#' @import data.table methods parallel pbapply
#' @export

setClass(Class = "peims", slots = c(seedi = "matrix", oir = "matrix", betaij = "matrix"))
