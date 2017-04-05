\name{binnedCounts}
\alias{binnedCounts}

\title{
Binned Frequency Distribution of a Numeric Variable
}

\description{
Bins a numeric variable, as for a histogram, and reports the counts in each bin. The computations are done by the \code{\link{hist}} function, but no histogram is drawn.
}

\usage{
binnedCounts(x, breaks = "Sturges")
}

\arguments{
  \item{x}{a numeric vector.}
  
  \item{breaks}{specification of the breaks between bins, to be passed to the \code{\link{hist}} function.}
}

\value{
A vector of counts, named with the end-points of the corresponding bins.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{hist}}
}

\examples{
if (require(car)){
    with(Prestige, binnedCounts(income))
    }
}

\keyword{univar}
