\name{Barplot}
\alias{Barplot}

\title{
Bar Plots
}

\description{
Create bar plots for one or two factors scaled by frequency or precentages.
In the case of two factors, the bars can be divided (stacked) or plotted in
parallel (side-by-side). This function is a front end to \code{\link{barplot}}
in the \pkg{graphics} package.}

\usage{
Barplot(x, by, scale = c("frequency", "percent"), 
  style = c("divided", "parallel"), col = rainbow_hcl(length(levels(by))), 
  xlab = deparse(substitute(x)), legend.title = deparse(substitute(by)), 
  ylab = scale, legend.pos = "topright", ...)
}

\arguments{
  \item{x}{
a factor.}
  \item{by}{
optionally, a second factor.}
  \item{scale}{
either \code{"frequency"} (the default) or \code{"percent"}.}
  \item{style}{
for two-factor plots, either \code{"divided"} (the default) or \code{"parallel"}.}
  \item{col}{
colors for the \code{by} factor in two-factor plots; defaults to colors
provided by \code{\link{rainbow_hcl}} in the \pkg{colorspace} package.}
  \item{xlab}{
an optional character string providing a label for the horizontal axis.}
  \item{legend.title}{
an optional character string providing a title for the legend.}
  \item{ylab}{
an optional character string providing a label for the vertical axis.}
  \item{legend.pos}{
position of the legend, in a form acceptable to the \code{\link{legend}}
function.}
  \item{...}{
arguments to be based to the \code{\link{barplot}} function.}
}

\value{
Returns \code{NULL} invisibly.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{barplot}}, \code{\link{legend}}, \code{\link{rainbow_hcl}}
}

\examples{
if (require(car)){
    data(Mroz)
    with(Mroz, {
      Barplot(wc)
      Barplot(wc, by=hc)
      Barplot(wc, by=hc, style="parallel", scale="percent")
    })
}
}

\keyword{hplot}
