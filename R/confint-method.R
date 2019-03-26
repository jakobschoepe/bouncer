#' @title Estimating confidence intervals for smoothed model parameters
#' @description For objects of class \code{"peims"}, \code{confint} estimates confidence intervals for smoothed model parameters from approximated sampling distributions.
#' @param object An object of class \code{"peims"}.
#' @param level A numeric value giving the level of confidence.
#' @param method A string indicating the method of estimation (see Details).
#' @details \code{confint} provides four methods to estimate confidence intervals for smoothed model parameters: bias-corrected smoothed interval (\code{"bcsi"}; default), percentile interval (\code{"pcti"}), smoothed interval (\code{"smoi"}), standard interval (\code{"stdi"}).
#' @return A matrix with columns giving the lower and upper confidence limits for each smoothed model parameter.
#' @references Efron B (2014) Estimation and accuracy after model selection. J Am Stat Assoc 109:991--1007
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
#' confint(fit)
#' @export

setMethod(f = "confint",
          signature = "peims",
          definition = function(object, level = .95, method = "bcsi") {
            if (!is.numeric(x = level)) {
              stop("\"level\" must be a numeric value")
            }
                    
            else if (length(x = level) != 1L) {       
              stop("single numeric value for \"level\" expected")          
            }
                    
            else if (!is.character(x = method)) {
              stop("\"method\" must be a character string")     
            }
                    
            else if (length(x = method) != 1L) {
              stop("single character string for \"method\" expected")
            }
            
            else if (!(method %in% c("bcsi", "pcti", "smoi", "stdi"))) {
              stop("\"method\" is misspecified. Currently available confidence interval estimation procedures are: \"bcsi\", \"pcti\", \"smoi\" and \"stdi\"")
            }
                    
            betaij <- object@betaij
            betaj <- colMeans(x = betaij, na.rm =TRUE)
            oir <- object@oir
            or <- colMeans(x = oir, na.rm = TRUE)
            alpha <- (1 - level) / 2
            j <- ncol(x = betaij)
            k <- nrow(x = betaij)
            p <- c(alpha, 1 - alpha)
            q <- qt(p = p, df = k - j)

            ci <- array(data = NA, dim = c(j, 2L), dimnames = list(names(x = betaj), paste(x = format(x = 100 * p, digits = 3, scientific = FALSE, trim = TRUE), "%", sep = "")))

            if (method == "bcsi") {
              o1 <- oir - rep(x = or, each = k)
              o2 <- oir - 1
              b1 <- betaij - rep(x = betaj, each = k)
              ci[] <- t(x = betaj + rep(x = q, each = j) * sapply(X = 1:j, FUN = function(i) {sqrt(x = sum(x = colSums(x = (o1 * b1[, i]) / k, na.rm = TRUE)^2) - 1/(k)^2 * sum(x = colSums(x = ((o2 * b1[, i]) - colMeans(x = o2 * b1[, i], na.rm = TRUE))^2, na.rm = TRUE)))}))
            }

            else if (method == "pcti") {
              ci[] <- t(x = sapply(X = 1:j, FUN = function(i) {quantile(x = betaij[, i], prob = p, na.rm = TRUE)}))
            }

            else if (method == "smoi") {
              o1 <- oir - rep(x = or, each = k)
              b1 <- betaij - rep(x = betaj, each = k)
              ci[] <- t(x = betaj + rep(x = q, each = j) * sapply(X = 1:j, FUN = function(i) {sqrt(x = sum(x = colSums(x = (o1 * b1[, i]) / k, na.rm = TRUE)^2))}))
            }

            else if (method == "stdi") {
              ci[] <- t(x = betaj + rep(x = q, each = j) * sapply(X= 1:j, FUN = function(i) {sd(x = betaij[, i], na.rm = TRUE)}))
            }

            return(ci)
          }
)
