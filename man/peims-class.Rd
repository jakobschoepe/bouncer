\alias{peims-class}
\alias{coef,peims-method}
\alias{confint,peims-method}
\alias{subset,peims-method}
\alias{unique,peims-method}
\author{Jakob Schöpe (\email{jas@med-imbei.uni-saarland.de})}
\description{}
\docType{class}
\examples{
showClass("peims")
}
\name{peims-class}
\Rdversion{1.1}
\section{Slots}{
  \describe{
    \item{\code{oir}:}{Object of class \code{"matrix"}}
    \item{\code{betaij}:}{Object of class \code{"matrix"}}
  }
}
\section{Methods}{
  \describe{
    \item{coef}{Estimate the expected value of each model parameter.}
    \item{confint}{Estimate the confidence limits for each model parameter.}
    \item{subset}{}
    \item{unique}{}
	 }
}
\title{S4 Class \code{"peims"}}
