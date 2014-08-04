\name{colPercents}
\alias{colPercents}
\alias{rowPercents}
\alias{totPercents}

\title{Row, Column, and Total Percentage Tables}
\description{
  Percentage a matrix or higher-dimensional array of frequency counts by
  rows, columns, or total frequency.
}
\usage{
colPercents(tab, digits=1)
rowPercents(tab, digits=1)
totPercents(tab, digits=1)
}

\arguments{
  \item{tab}{a matrix or higher-dimensional array of frequency counts.}
  \item{digits}{number of places to the right of the decimal place for percentages.}
}

\value{
  Returns an array of the same size and shape as \code{tab} percentaged by
  rows or columns, plus rows or columns of totals and counts, or by the
  table total.
}

\examples{
if (require(car)){
    data(Mroz) # from car package
    cat("\n\n column percents:\n")
    print(colPercents(xtabs(~ lfp + wc, data=Mroz)))
    cat("\n\n row percents:\n")
    print(rowPercents(xtabs(~ hc + lfp, data=Mroz)))
    cat("\n\n total percents:\n")
    print(totPercents(xtabs(~ hc + wc, data=Mroz)))
    cat("\n\n three-way table, column percents:\n")
    print(colPercents(xtabs(~ lfp + wc + hc, data=Mroz)))
}
}

\author{John Fox \email{jfox@mcmaster.ca}}


\keyword{misc}

