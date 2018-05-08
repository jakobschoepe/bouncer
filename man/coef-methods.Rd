\alias{coef-methods}
\alias{coef,peims-method}
\arguments{
  \item{object}{An object of class \code{"peims"}.}
}
\author{Jakob Schöpe (\email{jas@med-imbei.uni-saarland.de})}
\description{For objects of class \code{"peims"}, the \code{coef} function estimates the expected value of each model parameter from their approximated sampling distribution using bagging methodology.}
\docType{methods}
\examples{
f <- function(i, data, size, replace) {
  data <- data[sample(x = 1:nrow(x = data), size = size, replace = replace),]
  null <- glm(formula = mpg ~ 1, family = gaussian, data = data)
  full <- glm(formula = mpg ~ . -id, family = gaussian, data = data)
  fit <- coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
  return(list(oir = data$id, betaij = fit))
}

fit <- peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 1000L, seed = 123L, ncpus = 4L)

coef(fit)
}
\name{coef-methods}
\references{Schöpe J & Wagenpfeil S (2018)}
\title{Estimate the expected value of each model parameter}
\usage{\S4method{coef}{peims}(object)}
\value{A vector giving the expected value for each model parameter.}