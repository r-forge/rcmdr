\name{reshapeL2W}
\alias{reshapeL2W}

\title{
Reshape Repeated-Measures Data from Long to Wide Format
}

\description{
A simple front-end to the standard R \code{\link{reshape}} function. The data are assumed to be in "long" format, with several rows for each subject. 
}

\usage{
reshapeL2W(data, within, id, varying, ignore)
}

\arguments{
  \item{data}{a data frame in long format.}
  \item{within}{a character vector of names of the within-subjects factors in the long form of the data; there must be at least one within-subjects factor.}
  \item{id}{the (character) name of the variable representing the subject identifier in the long form of the data set; that is, rows with the same \code{id} belong to the same subject.}
  \item{varying}{a character vector of names of the occasion-varying variables in the long form of the data; there must be at least one such variable, and typically there will be just one, an occasion-varying response variable.}
  \item{ignore}{an optional character vector of names of variables in the long form of the data to exclude from the wide data set.}
}

\details{
Between-subjects variables don't vary by occasions for each subject. Variables that aren't listed explicitly in the arguments to the function are assumed to be between-subjects variables, and a warning is printed if their values aren't invariant for each subject (see the \code{ignore} argument). 

Within-subjects factors vary by occasions for each subject, and it is assumed that the within-subjects design is regular, completely crossed, and balanced, so that the same combinations of within-subjects factors are observed for each subject. 

Occasion-varying variables, as their name implies, (potentially) vary by occasions for each subject, and include one or more "response" variables, possibly along with occasion-varying covariates; these variables can be factors as well as numeric variables. 

The data are reshaped so that there is one row per subject, with columns for the between-subjects variables, and each occasion-varying variable as multiple columns representing the combinations of levels of the within-subjects factors. The names of the columns for the occasion-varying variables are composed from the combinations of levels of the within-subjects factors and from the names of the occasion-varying variables (if there is more than one).  If a subject in the long form of the data set lacks any combination of levels of within-subjects factors, he or she is excluded (with a warning) from the wide form of the data.
}

\value{a data frame in "wide" format, with one row for each subject, columns representing the between subjects factors, and columns for the occasion-varying variable(s) for each combination of within-subjects factors.}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link{reshape}}, \code{\link[carData]{OBrienKaiser}}, \code{\link[carData]{OBrienKaiserLong}}.}

\examples{
if (require(carData)){
  OBW <- reshapeL2W(OBrienKaiserLong,
                    within=c("phase", "hour"), 
                    id="id", varying="score")
  brief(OBW)
  # should be the same as OBrienKaiser in the carData package:
  all.equal(OBrienKaiser, OBW, check.attributes=FALSE)
}
}

\keyword{manip}
