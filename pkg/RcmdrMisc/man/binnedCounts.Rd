\name{binnedCounts}
\alias{binnedCounts}

\title{
Binned Frequency Distribution of Numeric Variables
}

\description{
Bins a numeric variable, as for a histogram, and reports the counts in each bin. The computations are done by the \code{\link{hist}} function, but no histogram is drawn. If supplied a numeric matrix or data frame, the distribution of each column is printed. 
}

\usage{
binnedCounts(x, breaks="Sturges", name=deparse(substitute(x)))
}

\arguments{
  \item{x}{a numeric vector, matrix, or data frame.}
  
  \item{breaks}{specification of the breaks between bins, to be passed to the \code{\link{hist}} function.}
  
  \item{name}{name for the variable; only used for vector argument \code{x}.}
}

\value{
For a numeric vector, invisibly returns the vector of counts, named with the end-points of the corresponding bins. For a matrix or data frame, invisibly returns \code{NULL}
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{hist}}
}

\examples{
with(Prestige, binnedCounts(income))
binnedCounts(Prestige[, 1:4])
}

\keyword{univar}
