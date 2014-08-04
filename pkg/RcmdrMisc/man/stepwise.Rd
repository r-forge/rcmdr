\name{stepwise}
\Rdversion{1.1}
\alias{stepwise}

\title{
Stepwise Model Selection
}

\description{
This function is a front end to the \code{\link{stepAIC}} function in the
\pkg{MASS} package.
}

\usage{
stepwise(mod, 
    direction = c("backward/forward", "forward/backward", "backward", "forward"), 
    criterion = c("BIC", "AIC"), ...)
}

\arguments{
  \item{mod}{a model object of a class that can be handled by \code{stepAIC}.}
  \item{direction}{if \code{"backward/forward"} (the default), selection starts with
    the full model and eliminates predictors one at a time, at each step considering whether the
    criterion will be improved by adding back in a variable removed at a previous step;
    if \code{"forward/backwards"}, selection starts with a model including only a constant,
    and adds predictors one at a time, at each step considering whether the criterion
    will be improved by removing a previously added variable; \code{"backwards"} and 
    \code{"forward"} are similar without the reconsideration at each step.}
  \item{criterion}{for selection. Either \code{"BIC"} (the default) or \code{"AIC"}. Note that
    \code{stepAIC} labels the criterion in the output as \code{"AIC"} regardless of which
    criterion is employed.}
  \item{...}{arguments to be passed to \code{stepAIC}.}
}

\value{
The model selected by \code{stepAIC}.
}

\references{
    W. N. Venables and B. D. Ripley
    \emph{Modern Applied Statistics Statistics with S, Fourth Edition}
    Springer, 2002.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link{stepAIC}}}

\examples{
# adapted from ?stepAIC in MASS
if (require(MASS)){
data(birthwt)
bwt <- with(birthwt, {
    race <- factor(race, labels = c("white", "black", "other"))
    ptd <- factor(ptl > 0)
    ftv <- factor(ftv)
    levels(ftv)[-(1:2)] <- "2+"
    data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
               ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
print(stepwise(birthwt.glm, trace = FALSE))
print(stepwise(birthwt.glm, direction="forward/backward"))
}
}

\keyword{models}
