\name{indexplot}
\alias{indexplot}

\title{
Index Plots
}

\description{
Index plots with point identification.
}

\usage{
indexplot(x, labels = seq_along(x), id.method = "y", type = "h", 
    id.n = 0, ylab, ...) 
}

\arguments{
  \item{x}{a numeric variable, a matrix whose columns are numeric variables, or a numeric data frame;
    if \code{x} is a matrix or data frame, plots vertically aligned index plots for the columns.}
  \item{labels}{point labels; if \code{x} is a data frame, defaults to the row names of \code{x},
    otherwise to the case index.}
  \item{id.method}{method for identifying points; see \code{\link[car]{showLabels}}.}
  \item{type}{to be passed to \code{\link{plot}}.}
  \item{id.n}{number of points to identify; see \code{\link[car]{showLabels}}.}
  \item{ylab}{label for vertical axis; if missing, will be constructed from \code{x}; for a data frame,
    defaults to the column names.}
  \item{\dots}{to be passed to \code{plot}.}
}

\value{
Returns labelled indices of identified points or (invisibly) \code{NULL} if no points
are identified or if there are multiple variables with some missing data.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link[car]{showLabels}}, \code{\link{plot.default}}
}

\examples{
if (require("car")){
    with(Prestige, indexplot(income, id.n=2, labels=rownames(Prestige)))
    indexplot(Prestige[, c("income", "education", "prestige")], id.n=2)
}
}

\keyword{hplot}
