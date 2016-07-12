\name{plotDistr}
\alias{plotDistr}

\title{
Plot a probability density, mass, or distribution function.
}
\description{
This function plots a probability density, mass, or distribution function, adapting the form
of the plot as appropriate.
}
\usage{
plotDistr(x, p, discrete=FALSE, cdf=FALSE, 
  regions=NULL, col="gray", ...)
}

\arguments{
  \item{x}{horizontal coordinates}
  \item{p}{vertical coordinates}
  \item{discrete}{is the random variable discrete?}
  \item{cdf}{is this a cumulative distribution (as opposed to mass) function?}
  \item{regions, col}{for continuous distributions only,
    if non-\code{NULL}, a list of regions to fill with color \code{col};
    each element of the list is a pair of \code{x} values with the minimum and maximum
    horizontal coordinates of the corresponding region.}
  \item{\dots}{arguments to be passed to \code{plot}.}
}

\value{
  Produces a plot; returns \code{NULL} invisibly.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\examples{
x <- seq(-4, 4, length=100)
plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density")
plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density",
    region=list(c(1.96, Inf), c(-Inf, -1.96)), col="red")
plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density",
    region=list(c(qnorm(0), qnorm(.025)), c(qnorm(.975), qnorm(1)))) # same 

x <- 0:10
plotDistr(x, pbinom(x, 10, 0.5), xlab="successes", 
    discrete=TRUE, cdf=TRUE,
    main="Binomial Distribution Function, p=0.5, n=10")
}

\keyword{hplot}
