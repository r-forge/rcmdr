\name{readSAS}
\alias{readSAS}
\title{
Read a SAS b7dat Data Set
}
\description{
\code{readSAS} reads a SAS ``b7dat'' data set, stored in a file of type \code{.sas7bdat}, into an R data frame; it provides
a front end to the \code{\link{read_sas}} function in the \pkg{readxl} package.
}

\usage{
readSAS(file, rownames=FALSE, 
       stringsAsFactors=default.stringsAsFactors())
}

\arguments{
  \item{file}{path to a SAS b7dat file.}
  \item{rownames}{if \code{TRUE} (the default is \code{FALSE}), the first column in the
    spreadsheet contains row names (which must be unique---i.e., no duplicates).}
  \item{stringsAsFactors}{if \code{TRUE} then columns containing character data are converted to
    factors; the default is taken from \code{default.stringsAsFactors()}.}
}

\value{
a data frame
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{read_sas}}
}

\keyword{manip}
