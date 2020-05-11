\name{readXL}

\alias{readXL}
\alias{excel_sheets}
\title{
Read an Excel File
}
\description{
\code{readXL} reads an Excel file, either of type \code{.xls} or \code{.xlsx} into an R data frame; it provides
a front end to the \code{\link{read_excel}} function in the \pkg{readxl} package.
\code{\link[readxl]{excel_sheets}} is re-exported from the \pkg{readxl} package and reports the
names of spreadsheets in an Excel file.}

\usage{
readXL(file, rownames = FALSE, header = TRUE, na = "", sheet = 1, 
  stringsAsFactors = FALSE)
  
excel_sheets(path)
}

\arguments{
  \item{file, path}{path to an Excel file.}
  \item{rownames}{if \code{TRUE} (the default is \code{FALSE}), the first column in the
    spreadsheet contains row names (which must be unique---i.e., no duplicates).}
  \item{header}{if \code{TRUE} (the default), the first row in the spreadsheet contains column
    (variable) names.}
  \item{na}{character string denoting missing data; the default is the empty string, \code{""}.}
  \item{sheet}{number of the spreadsheet in the file containing the data to be read; the
    default is \code{1}.}
  \item{stringsAsFactors}{if \code{TRUE} (the default is \code{FALSE}) then columns containing character data are converted to
    factors.}
}

\value{
a data frame
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{read_excel}}, \code{\link[readxl]{excel_sheets}}
}

\keyword{manip}
