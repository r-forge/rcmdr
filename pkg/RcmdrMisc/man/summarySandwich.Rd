\name{summarySandwich}
\alias{summarySandwich}
\alias{summarySandwich.lm}

\title{Linear Model Summary with Sandwich Standard Errors}
\description{
\code{summarySandwich} creates a summary of a \code{"lm"} object similar to the standard one,
with sandwich estimates of the coefficient standard errors in the place of the usual OLS standard errors,
also modifying as a consequence the reported t-tests and p-values for the coefficients.
Standard errors may be computed from a heteroscedasticity-consistent ("HC") covariance matrix for the
coefficients (of several varieties), or from a heteroscedasticity-and-autocorrelation-consistent  ("HAC") covariance matrix.
}
\usage{
summarySandwich(model, ...)
    
\method{summarySandwich}{lm}(model, 
  type=c("hc3", "hc0", "hc1", "hc2", "hc4", "hac"), ...)
}

\arguments{
  \item{model}{a linear-model object.}
  \item{type}{type of sandwich standard errors to be computed; see \code{\link{hccm}}
    in the \pkg{car} package, and \code{\link{vcovHAC}} in the \pkg{sandwich} package, for details.}
  \item{...}{arguments to be passed to \code{hccm} or \code{vcovHAC}}
}

\value{
an object of class \code{"summary.lm"}, with sandwich standard errors substituted for the
usual OLS standard errors; the omnibus F-test is similarly adjusted.
}

\author{John Fox \email{jfox@mcmaster.ca}}


\seealso{\code{\link[car]{hccm}}, \code{\link[sandwich]{vcovHAC}}.}

\examples{
    mod <- lm(prestige ~ income + education + type, data=Prestige)
    summary(mod)
    summarySandwich(mod)
}

\keyword{misc}

