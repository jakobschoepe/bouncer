\alias{unique-methods}
\alias{unique,peims-method}
\arguments{
  \item{x}{An object of class \code{"peims"}.}
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

unique(fit)
}
\name{unique-methods}
\references{Coming soon.}
\title{}
\usage{\S4method{unique}{peims}(x)}
\value{}