\name{readSPSS}
\alias{readSPSS}
\title{
Read an SPSS Data Set
}
\description{
\code{readStata} reads an SPSS data set, stored in a file of type \code{.sav} or \code{.por}, into an R data frame; it provides
a front end to the \code{\link{read_spss}} function in the \pkg{haven} package.
}

\usage{
readSPSS(file, rownames=FALSE, stringsAsFactors=default.stringsAsFactors(),
  tolower=TRUE)
}

\arguments{
  \item{file}{path to an SPSS \code{.sav} or \code{.por} file.}
  \item{rownames}{if \code{TRUE} (the default is \code{FALSE}), the first column in the
    data set contains row names, which should be unique.}
  \item{stringsAsFactors}{if \code{TRUE} then columns containing character data are converted to
    factors and factors are created from Stata value labels; 
    the default is taken from \code{default.stringsAsFactors()}.}
  \item{tolower}{change variable names to lowercase, default \code{TRUE}.}
}

\value{
a data frame
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{read_spss}}
}

\keyword{manip}
