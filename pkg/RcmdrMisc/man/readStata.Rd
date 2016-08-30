\name{readStata}
\alias{readStata}
\title{
Read a SAS b7dat Data Set
}
\description{
\code{readStata} reads a Stata data set, stored in a file of type \code{.dta}, into an R data frame; it provides
a front end to the \code{\link{read.dta13}} function in the \pkg{readstata13} package.
}

\usage{
readStata(file, rownames=FALSE, stringsAsFactors=default.stringsAsFactors(), 
    convert.dates=TRUE)
}

\arguments{
  \item{file}{path to a Stata \code{.dta} file.}
  \item{rownames}{if \code{TRUE} (the default is \code{FALSE}), the first column in the
    data set contains row names, which should be unique.}
  \item{stringsAsFactors}{if \code{TRUE} then columns containing character data are converted to
    factors and factors are created from Stata value labels; 
    the default is taken from \code{default.stringsAsFactors()}.}
  \item{convert.dates}{if \code{TRUE} (the default) then Stata dates are converted to R dates.}
}

\value{
a data frame
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{read.dta13}}
}

\keyword{manip}
