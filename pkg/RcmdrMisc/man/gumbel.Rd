\name{Gumbel}
\alias{Gumbel}
\alias{dgumbel}
\alias{pgumbel}
\alias{qgumbel}
\alias{rgumbel}

\title{
The Gumbel Distribution
}
\description{
Density, distribution function, quantile function and random generation for the Gumbel distribution with
specified \code{location} and \code{scale} parameters.
}

\usage{
dgumbel(x, location = 0, scale = 1)
pgumbel(q, location=0, scale=1, lower.tail=TRUE)
qgumbel(p, location=0, scale=1, lower.tail=TRUE)
rgumbel(n, location=0, scale=1)
}

\arguments{
  \item{x, q}{vector of quantiles (values of the variable).}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n)} > 1, the length is taken to be the number required.}
  \item{location}{location parameter (default \code{0}); potentially a vector.}
  \item{scale}{scale parameter (default \code{1}); potentially a vector.}
  \item{lower.tail}{logical; if \code{TRUE} (the default) probabilities and quantiles correspond to
     \eqn{P(X \le x)}, if \code{FALSE} to  \eqn{P(X > x)}.}
}

\references{
See \url{https://en.wikipedia.org/wiki/Gumbel_distribution} for details of the Gumbel distribution.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\examples{
x <- 100 + 5*c(-Inf, -1, 0, 1, 2, 3, Inf, NA)
dgumbel(x, 100, 5)
pgumbel(x, 100, 5)

p <- c(0, .25, .5, .75, 1, NA)
qgumbel(p, 100, 5)

summary(rgumbel(1e5, 100, 5))
}

\keyword{distribution}
