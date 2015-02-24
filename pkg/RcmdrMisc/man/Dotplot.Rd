\name{Dotplot}
\alias{Dotplot}

\title{
Dot Plots
}
\description{
Dot plot of numeric variable, either using raw values or binned, 
optionally classified by a factor. Dot plots are useful for visualizing the
distrbution of a numeric variable in a small data set.}

\usage{
Dotplot(x, by, bin = FALSE, breaks,
  xlim, xlab = deparse(substitute(x)), main = "", cex, correction=1/3)
}

\arguments{
  \item{x}{a numeric variable.}
  \item{by}{optinally a factor by which to classify \code{x}.}
  \item{bin}{if \code{TRUE} (the default is \code{FALSE}), \code{x} is binned prior to plotting.}
  \item{breaks}{
breaks for the bins, in a form acceptable to the \code{\link{hist}} function;
the default is \code{"Sturges"}.}
  \item{xlim}{optional 2-element numeric vector giving limits of the horizontal axis.}
  \item{xlab}{optional character string to label horizontal axis.}
  \item{main, cex, correction}{should not normally be specified by the user.}
}

\details{
If the \code{by} argument is specified, then one dot plot is produced for each
level of \code{by}; these are arranged vertically and all use the same scale
for \code{x}. An attempt is made to adjust the size of the dots to the space
available without making them too big.
}

\value{
Returns \code{NULL} invisibly.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{hist}}
}

\examples{
if (require(car)){
  data(Duncan)
  with(Duncan, {
    Dotplot(education)
    Dotplot(education, bin=TRUE)
    Dotplot(education, by=type)
    Dotplot(education, by=type, bin=TRUE)
  })
}

}

\keyword{hplot}
