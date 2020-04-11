\name{reshapeL2W}
\alias{reshapeL2W}

\title{
Reshape Repeated-Measures Data from Long to Wide Format
}

\description{
A simple front-end to the standard R \code{\link{reshape}} function. The data are assumed to be in "wide" format, with several rows for each subject. 
}

\usage{
reshapeL2W(data, within.names, id, responses)
}

\arguments{
  \item{data}{a data frame in long format.}
  \item{within.names}{a character vector of names of the within-subjects factors in the long form of the data; there must be at least one within-subjects factor.}
  \item{id}{the (character) name of the variable representing the subject identifier in the long form of the data set; that is, rows with the same \code{id} belong to the same subject.}
  \item{responses}{a character vector of names of the response variables in the long form of the data; there must be at least one response variable, and typically there will be just one.}
}

\details{
Between-subjects variables don't vary by occasions for each subject. Within-subjects factors vary by occasions for each subject, and it is assumed that the within-subjects design is regular, completely crossed, and balanced, so that the same combinations of within-subjects factors are observed for each subject. Response variables (potentially) vary by occasions for each subject. 

The data are reshaped so that there is one row per subject, with columns for the between-subjects variables, and the responses as multiple columns representing the combinations of levels of the within-subjects factors.If a subject in the long form of the data set lacks any combinations of within-subjects factors, he or she is excluded (with a warning) from the wide form of the data. The names of the response columns are composed from the combinations of levels of the within-subjects factors and from the names of the responses (if there is more than one).
}

\value{a data frame in "wide" format, with one row for each subject, columns representing the between subjects factors, and columns for the response(s) for each combination of within-subjects factors.}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link{reshape}}, \code{\link[carData]{OBrienKaiser}}, \code{\link[carData]{OBrienKaiserLong}}.}

\examples{
if (require(carData)){
  OBW <- reshapeL2W(OBrienKaiserLong,
                    within.names=c("phase", "hour"), 
                    id="id", responses="score")
  brief(OBW)
  # should be the same as OBrienKaiser in the carData package:
  all.equal(OBrienKaiser, OBW, check.attributes=FALSE)
}
}

\keyword{manip}
