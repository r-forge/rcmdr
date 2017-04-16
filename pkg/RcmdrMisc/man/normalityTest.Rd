\name{normalityTest}
\alias{normalityTest}
\alias{normalityTest.default}
\alias{normalityTest.formula}

\title{
Normality Tests
}
\description{
Perform one of several tests of normality, either for a variable or for a variable by groups. The \code{normalityTest}
function uses the \code{\link{shapiro.test}} function or one of several functions in the \pkg{nortest} package.
If tests are done by groups, then adjusted p-values, computed by the Holm method, are also reported (see \code{\link{p.adjust}}).
}

\usage{
normalityTest(x, ...)

\method{normalityTest}{formula}(formula, test, data, ...)

\method{normalityTest}{default}(x, 
    test=c("shapiro.test", "ad.test", "cvm.test", "lillie.test", 
        "pearson.test", "sf.test"),
    groups, vname, gname, ...)
}

\arguments{
  \item{x}{numeric vector or formula.}
  \item{formula}{one-sided formula of the form \code{~x} or two-sided formula of the form \code{x ~ groups},
    where \code{x} is a numeric variable and \code{groups} is a factor.}
  \item{data}{a data frame containing the data for the test.}
  \item{test}{quoted name of the function to perform the test.}
  \item{groups}{optional factor to divide the data into groups.}
  \item{vname}{optional name for the variable; if absent, taken from \code{x}.}
  \item{gname}{optional name for the grouping factor; if absent, taken from \code{groups}.}
  \item{\dots}{any arguments to be passed down; the only useful such arguments are for the 
    \code{\link{pearson.test}} function in the \pkg{nortest} package.}
}

\value{
If testing by groups, the function invisibly returns \code{NULL}; otherwise it returns an object of class
\code{"htest"}, which normally would be printed.
}

\author{John Fox \email{jfox@mcmaster.ca}}


\seealso{\code{\link{shapiro.test}}, \code{\link{ad.test}}, \code{\link{cvm.test}}, \code{\link{lillie.test}},
  \code{\link{pearson.test}}, \code{\link{sf.test}}.}

\examples{
  data(Prestige, package="car")
  with(Prestige, normalityTest(income))
  normalityTest(income ~ type, data=Prestige, test="ad.test")
  normalityTest(~income, data=Prestige, test="pearson.test", n.classes=5)
}

\keyword{htest}
