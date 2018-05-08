\alias{peims}
\arguments{
  \item{f}{A user-defined function (see Details).}
  \item{data}{A data frame containing the variables in the model.}
  \item{size}{A positive integer giving the number of observations to choose.}
  \item{replace}{A logical constant indicating if sampling should be with replacement.}
  \item{k}{A positive integer giving the number of resampling replicates.}
  \item{seed}{An integer giving the random seed to initialize Pierre L'Ecuyer's multiple streams of pseudo-random numbers.}
  \item{ncpus}{A positive integer giving the number of cores to be used during processing (\code{ncpus = 1} for serial processing is not recommended)}
  \item{pkgs}{An optional character vector giving the names of the required packages.}
  \item{\dots}{Optional arguments}
}
\author{Jakob Schöpe (\email{jas@med-imbei.uni-saarland.de})}
\description{}
\details{}
\examples{
f <- function(i, data, size, replace) {
  data <- data[sample(x = 1:nrow(x = data), size = size, replace = replace),]
  null <- glm(formula = mpg ~ 1, family = gaussian, data = data)
  full <- glm(formula = mpg ~ . -id, family = gaussian, data = data)
  fit <- coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
  return(list(oir = data$id, betaij = fit))
}

fit <- peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 1000L, seed = 123L, ncpus = 4L)
}
\name{peims}
\references{Coming soon.}                                                                                           
\title{}
\usage{peims(f, data, size, replace, k, seed, ncpus, pkgs, ...)}
\value{
\item{oir}{A matrix containing the number of draws per observation in each resampling replicate.}
\item{betaij}{A matrix containing the estimated model parameters of each resampling replicate.}
}


