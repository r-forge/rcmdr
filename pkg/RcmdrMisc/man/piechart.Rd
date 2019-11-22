\name{piechart}
\alias{piechart}

\title{
Draw a Piechart With Percents or Counts in the Labels
}
\description{
\code{piechart} is a front-end to the standard R \code{\link{pie}} function, with the
capability of adding percents or counts to the pie-segment labels.}
\usage{
piechart(x, scale = c("percent", "frequency", "none"), 
  col = rainbow_hcl(nlevels(x)), ...)
}

\arguments{
  \item{x}{
a factor or other discrete variable; the segments of the pie correspond to the
unique values (levels) of \code{x} and are proportional to the frequency counts in
the various levels.
}
  \item{scale}{
parenthetical numbers to add to the pie-segment labels; the default is \code{"percent"}.
}
  \item{col}{
colors for the segments; the default is provided by the \code{\link[colorspace]{rainbow_hcl}}
function in the \pkg{colorspace} package.
}
  \item{\dots}{
further arguments to be passed to \code{\link{pie}}.
}
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{pie}}, \code{\link[colorspace]{rainbow_hcl}}
}

\examples{
with(Duncan, piechart(type))
}

\keyword{hplot}
