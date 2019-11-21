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
Barplot(x, by, scale = c("frequency", "percent"), conditional=TRUE,
  style = c("divided", "parallel"), 
  col=if (missing(by)) "gray" else rainbow_hcl(length(levels(by))),
  xlab = deparse(substitute(x)), legend.title = deparse(substitute(by)), 
  ylab = scale, main=NULL, legend.pos = "above", label.bars=TRUE, ...)
}

\arguments{
  \item{x}{
a factor.}
  \item{by}{
optionally, a second factor.}
  \item{scale}{
either \code{"frequency"} (the default) or \code{"percent"}.}
  \item{conditional}{
if \code{TRUE} then percentages are computed separately for each value of \code{x} 
(i.e., conditional percentages of \code{by} within levels of \code{x}); 
if \code{FALSE} then total percentages are graphed; 
ignored if \code{scale="frequency"}.}
  \item{style}{
for two-factor plots, either \code{"divided"} (the default) or \code{"parallel"}.}
  \item{col}{
if \code{by} is missing, the color for the bars, defaulting to \code{"gray"};
otherwise colors for the levels of the \code{by} factor in two-factor plots, defaulting to colors
provided by \code{\link{rainbow_hcl}} in the \pkg{colorspace} package.}
  \item{xlab}{
an optional character string providing a label for the horizontal axis.}
  \item{legend.title}{
an optional character string providing a title for the legend.}
  \item{ylab}{
an optional character string providing a label for the vertical axis.}
  \item{main}{
an optional main title for the plot.}
  \item{legend.pos}{
position of the legend, in a form acceptable to the \code{\link{legend}}
function; the default, \code{"above"}, puts the legend above the plot.}
  \item{label.bars}{
if \code{TRUE} (the default) show values of frequencies or percents in the bars.
  }
  \item{...}{
arguments to be passed to the \code{\link{barplot}} function.}
}

\value{
Invisibly returns the horizontal coordinates of the centers of the bars.
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
      Barplot(wc, col="lightblue")
      Barplot(wc, by=hc)
      Barplot(wc, by=hc, scale="percent")
      Barplot(wc, by=hc, style="parallel", 
        scale="percent", legend.pos="center")
    })
}
}

\keyword{hplot}
