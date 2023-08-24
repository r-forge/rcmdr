\name{numSummary}
\alias{numSummary}
\alias{print.numSummary}
\alias{CV}

\title{Summary Statistics for Numeric Variables}
\description{
\code{numSummary} creates neatly formatted tables of means, standard deviations, coefficients of variation,
skewness, kurtosis, and quantiles of numeric variables. \code{CV} computes the coefficient of variation.
}
\usage{
numSummary(data, 
           statistics=c("mean", "sd", "se(mean)", "var", "CV", "IQR", 
                "quantiles", "skewness", "kurtosis"),
            type=c("2", "1", "3"),
            quantiles=c(0, .25, .5, .75, 1),
            groups)

CV(x, na.rm=TRUE)
    
\method{print}{numSummary}(x, ...)
}

\arguments{
  \item{data}{a numeric vector, matrix, or data frame.}
  \item{statistics}{any of \code{"mean"}, \code{"sd"}, \code{"se(mean)"},
  \code{"var"}, \code{"CV"}, \code{"IQR"}, \code{"quantiles"}, 
  	\code{"skewness"}, or \code{"kurtosis"},
    defaulting to \code{c("mean", "sd", "quantiles", "IQR")}.}
  \item{type}{definition to use in computing skewness and kurtosis; see the 
  	\code{\link[e1071]{skewness}} and \code{\link[e1071]{kurtosis}} functions in the \pkg{e1071}
  	package. The default is \code{"2"}.}
  \item{quantiles}{quantiles to report; default is \code{c(0, 0.25, 0.5, 0.75, 1)}.}
  \item{groups}{optional variable, typically a factor, to be used to partition the data.}
  \item{x}{object of class \code{"numSummary"} to print, or for \code{CV}, a numeric vector or matrix.}
  \item{na.rm}{if \code{TRUE} (the default) remove \code{NA}s before computing the coefficient of variation.}
  \item{\dots}{arguments to pass down from the print method.}
}

\value{
\code{numSummary} returns an object of class \code{"numSummary"} containing the table of 
statistics to be reported along with information on missing data, if there are any. \code{CV} returns the coefficient(s) of variation.
}

\author{John Fox \email{jfox@mcmaster.ca}}


\seealso{\code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[stats]{quantile}},
	\code{\link[e1071]{skewness}}, \code{\link[e1071]{kurtosis}}.}

\examples{
if (require("carData")){
    data(Prestige)
    Prestige[1, "income"] <- NA
    print(numSummary(Prestige[,c("income", "education")], 
    	statistics=c("mean", "sd", "quantiles", "cv", "skewness", "kurtosis")))
    print(numSummary(Prestige[,c("income", "education")], groups=Prestige$type))
    remove(Prestige)
}
}

\keyword{misc}

