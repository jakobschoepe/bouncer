#' @title S4 Class \code{"bouncer"}
#' @slot seedi A numeric matrix containing the state of L'Ecuyer's pseudo-random number generator from each resampling replication.
#' @slot oir A numeric matrix containing the frequency of draws per observation from each resampling replication.
#' @slot betaij A numeric matrix containing the model parameter estimates from each resampling replication.
#' @import data.table methods parallel pbapply
#' @export

setClass(Class = "bouncer", slots = c(seedi = "matrix", oir = "matrix", betaij = "matrix"))
