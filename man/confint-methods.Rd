\alias{confint-methods}
\alias{confint,peims-method}
\arguments{
  \item{object}{An object of class \code{"peims"}.}
  \item{level}{}
  \item{method}{A string indicating the method of confidence interval estimation (see Details).}
}
\author{Jakob Schöpe (\email{jas@med-imbei.uni-saarland.de})}
\description{}
\details{}
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
\name{confint-methods}
\references{Coming soon.}
\title{Estimate the confidence limits for each model parameter}
\usage{\S4method{confint}{peims}(object, level = .95, method = "bcsi")}
\value{A matrix with columns giving the lower and upper confidence limits for each model parameter.}
