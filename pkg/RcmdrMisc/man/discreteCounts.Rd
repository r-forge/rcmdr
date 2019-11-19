\name{discreteCounts}
\alias{discreteCounts}

\title{
Frequency Distributions of Numeric Variables
}

\description{
Computes the frequency and percentage distribution of a descrete numeric variable or the distributions of the variables
in a numeric matrix or data frame.
}

\usage{
discreteCounts(x, round.percents=2, name=deparse(substitute(x)),
  max.values=min(round(2*sqrt(length(x))), round(10*log10(length(x))), 100))
}

\arguments{

  \item{x}{a discrete numeric vector, matrix, or data frame.}
  
  \item{round.percents}{number of decimal places to round percentages; default is \code{2}.}
  
  \item{name}{name for the variable; only used for vector argument \code{x}.}
  
  \item{max.values}{maximum number of unique values (default is the smallest of 
  twice the square root of the number of elements in \code{x}, 10 times the log10 of
  the number of elements, and \code{100}); 
  if exceeded, an error is reported.}
  
}

\value{
For a numeric vector, invisibly returns the table of counts. 
For a matrix or data frame, invisibly returns \code{NULL}
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{binnedCounts}}
}

\examples{
set.seed(12345) # for reproducibility
discreteCounts(data.frame(x=rpois(51, 2), y=rpois(51, 10)))
}

\keyword{univar}
