\name{readXL}
\alias{readXL}
\title{
Read an Excel File
}
\description{
Read and Excel file, either of type \code{.xls} or \code{.xlsx} into an R data frame; provides
a front end to the \code{\link{read_excel}} function in the \pkg{readxl} package.}

\usage{
readXL(file, rownames = FALSE, header = TRUE, na = "", sheet = 1, 
  stringsAsFactors = default.stringsAsFactors())
}

\arguments{
  \item{file}{path to an Excel file.}
  \item{rownames}{if \code{TRUE} (the default is \code{FALSE}), the first column in the
    spreadsheet contains row names.}
  \item{header}{if \code{TRUE} (the default), the first row in the spreadsheet contains column
    (variable) names.}
  \item{na}{character string denoting missing data; the default is the empty string, \code{""}.}
  \item{sheet}{number of the spreadsheet in the file containing the data to be read; the
    default is \code{1}.}
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
\code{\link{read_excel}}
}

\keyword{manip}
