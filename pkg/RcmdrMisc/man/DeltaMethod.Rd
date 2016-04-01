\name{DeltaMethod}
\alias{DeltaMethod}
\alias{print.DeltaMethod}

\title{Confidence Intervals by the Delta Method}

\description{
\code{DeltaMethod} is a wrapper for the \code{\link[car]{deltaMethod}} function in the \pkg{car} package. It computes the asymptotic standard error of an arbitrary, usually nonlinear, function of model coefficients, which are named \code{b0} (if there is an intercept in the model), \code{b1}, \code{b2}, etc., and based on the standard error, a confidence interval based on the normal distribution.
}

\usage{
DeltaMethod(model, g, level = 0.95)
\method{print}{DeltaMethod}(x, ...)
}

\arguments{
  \item{model}{a regression model; see the \code{\link[car]{deltaMethod}} documentation.}
  \item{g}{the expression --- that is, function of the coefficients --- to evaluate, as a character string.}
  \item{level}{the confidence level, defaults to \code{0.95}.}
  \item{x}{an object of class \code{"DeltaMethod"}.}
  \item{...}{optional arguments to pass to \code{print} to show the results.}
}

\value{
\code{DeltaMethod} returns an objects of class \code{"DeltaMethod"}, for which a \code{print} method is provided.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{\code{\link[car]{deltaMethod}} function in the \pkg{car} package}

\examples{
if (require(car)){
  DeltaMethod(lm(prestige ~ income + education, data=Duncan), "b1/b2")
  }
}

\keyword{models}
