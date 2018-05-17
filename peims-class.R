#' @title S4 Class \code{"peims"}
#' @description empty
#' @import data.table methods parallel pbapply
#' @export

setClass(Class = "peims", slots = c(oir = "matrix", betaij = "matrix"))
