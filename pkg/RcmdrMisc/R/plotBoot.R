# last modified 2017-02-07

plotBoot <- function(object, confint=NULL, ...){
  UseMethod("plotBoot")
}

plotBoot.boot <- function(object, confint=NULL, ...){
  mfrow <- function (n) {
    rows <- round(sqrt(n))
    cols <- ceiling(n/rows)
    c(rows, cols)
  }
  if (is.null(confint)) confint <- confint(object)
  t0 <- object$t0
  t <- object$t
  if (any(is.na(t))){
    t <- na.omit(t)
    warning("bootstrap samples with missing parameter values suppressed")
  }
  npars <- length(t0)
  pars <- names(t0)
  savepar <- par(mfrow=mfrow(npars), oma=c(0, 0, 2, 0), mar=c(5.1, 4.1, 2.1, 2.1))
  on.exit(par(savepar))
  for (i in 1:npars){
    car::densityPlot(t[, i], xlab=pars[i], method="adaptive")
    abline(v=t0[i], lty=2, col="blue")
    abline(v=confint[i, ], lty=2, col="magenta")
  }
  title(main="Bootstrap Distributions", outer=TRUE, line=0.5)
}
