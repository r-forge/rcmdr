\name{readSPSS}
\alias{readSPSS}
\title{
Read an SPSS Data Set
}
\description{
\code{readSPSS} reads an SPSS data set, stored in a file of type \code{.sav} or \code{.por}, into an R data frame; it provides
a front end to the \code{\link{read_spss}} function in the \pkg{haven} package and the \code{\link{read.spss}} function in the \pkg{foreign} package.
}

\usage{
readSPSS(file, rownames=FALSE, stringsAsFactors=default.stringsAsFactors(),
  tolower=TRUE, use.haven=!por)
}

\arguments{
  \item{file}{path to an SPSS \code{.sav} or \code{.por} file.}
  \item{rownames}{if \code{TRUE} (the default is \code{FALSE}), the first column in the
    data set contains row names, which should be unique.}
  \item{stringsAsFactors}{if \code{TRUE} then columns containing character data are converted to
    factors and factors are created from SPSS value labels; 
    the default is taken from \code{default.stringsAsFactors()}.}
  \item{tolower}{change variable names to lowercase, default \code{TRUE}.}
  \item{use.haven}{use \code{\link{read_spss}} from the \pkg{haven} package to read the file,
    in preference to \code{\link{read.spss}} from the \pkg{foreign} package; the default is
    \code{TRUE} for a \code{.sav} file and \code{FALSE} for a \code{.por} file.}
}

\value{
a data frame
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{read_spss}}, \code{\link{read.spss}}
}

\keyword{manip}
