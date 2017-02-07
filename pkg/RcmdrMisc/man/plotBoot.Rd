\name{plotBoot}
\alias{plotBoot}
\alias{plotBoot.boot}
\title{
Plot Bootstrap Distributions
}
\description{
The function takes an object of class \code{"boot"} and creates an array of density estimates for the bootstrap distributions of the parameters.
}

\usage{
plotBoot(object, confint=NULL, ...)
\method{plotBoot}{boot}(object, confint=NULL, ...) 
}

\arguments{
  \item{object}{an object of class \code{"boot"}.}
  \item{confint}{an object of class \code{"confint.boot"} (or an ordinary 2-column matrix) containing confidence limits for the parameters in \code{object}; if \code{NULL} (the default), these are computed from the first argument, using the defaults for \code{"boot"} objects.}
  \item{\dots}{not used}
}

\details{
Creates an array of adaptive kernal density plots, using \code{\link[car]{densityPlot}} in the \pkg{car} package, showing the bootstrap distribution, point estimate ,and (optionally) confidence limits for each parameter.}

\value{
Invisibly returns the object produced by \code{densityPlot}.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link[car]{densityPlot}}
}

\examples{
\dontrun{
plotBoot(Boot(lm(prestige ~ income + education + type, data=Duncan)))
}
}

\keyword{hplot}
