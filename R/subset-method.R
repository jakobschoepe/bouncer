#' @title Subsetting a model obtained from resampling
#' @description For objects of class \code{"peims"}, \code{subset()} returns a model of interest.
#' @param x An object of class \code{"peims"}.
#' @param model An integer giving the index of the observed ranking of the model of interest among all unique models (\code{model = 1} for the most frequent unique model).
#' @return An object of S4 class \code{"peims"} containing the following slots:
#' \item{oir}{A matrix containing the number of draws per observation in each resampling replicate with the model of interest.}
#' \item{betaij}{A matrix containing the model parameter estimates of each resampling replicate with the model of interest.}
#' @aliases coef,peims-method
#' @author Jakob Schöpe
#' @examples
#' f <- function(data) {
#' null <- glm(formula = y ~ 1, family = binomial, data = data)
#' full <- glm(formula = y ~ ., family = binomial, data = data)
#' fit <- coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
#' return(fit)
#' }
#'
#' fit <- peims(f = f, data = data, size = 100L, replace = TRUE, k = 5000L, seed = 123L, ncpus = 2L)
#'
#' subset(fit)
#' @export

setMethod(f = "subset",
          signature = "peims",
          definition = function(x, model = 1) {

            # Cast matrices into data tables to subsequently subset more efficient
            betaij <- data.table::as.data.table(x = x@betaij)
            oir <- data.table::as.data.table(x = x@oir)

            # ???
            betaij[, m := do.call(what = paste0, args = lapply(X = .SD, FUN = function(x) {+is.na(x = x)}))]
            oir[, m := betaij[["m"]]]
            tmp <- betaij[, .N, by = m][order(N, decreasing = TRUE), i := .I][, setorder(x = .SD, cols = i)]

            # Subset data tables conditional on argument 'model'
            betaij <- as.matrix(x = betaij[tmp[i == model, m], on = .(m), nomatch = 0, !"m"][, Filter(f = function(x) {!anyNA(x = x)}, x = .SD)])
            oir <- as.matrix(x = oir[tmp[i == model, m], on = .(m), nomatch = 0, !"m"])

            return(new(Class = "peims", oir = oir, betaij = betaij))
          }
)
