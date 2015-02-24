Dotplot <- function(x, by, bin=FALSE, breaks, xlim,
                    xlab=deparse(substitute(x)), main="", cex, correction=1/3){
  # the following is a size tuning factor as is correction
  correction.height <- if (missing(by)) 1 else 1/2
  force(xlab)
  if (missing(by)){
    x <- na.omit(x)
  }
  else{
    label.by <- deparse(substitute(by))
    keep <- complete.cases(x, by)
    x <- x[keep]
    by <- by[keep]
  }
  if (missing(xlim)) xlim <- range(x)
  force(xlab)
  if (bin) hist <- hist(x, breaks=if(missing(breaks)) "Sturges" else breaks,
                        plot=FALSE)
  if (missing(by)){
    y <- if (bin) hist$counts else table(x)
    x <- if (bin) hist$mids else sort(unique(x))
    plot(range(x), 0:1, type="n", xlab=xlab, ylab="", main=main, axes=FALSE,
         xlim=xlim)
    y.limits <- par("usr")[3:4]
    char.height <- correction.height*par("cxy")[2]
    axis(1, pos=0)
    abline(h=0)
    if (missing(cex)) cex <- min(((y.limits[2] - y.limits[1])/char.height)/
                                   max(y), 2)
    for (i in 1:length(y)){
      if (y[i] == 0) next
      points(rep(x[i], y[i]), cex*correction*char.height*seq(1, y[i]), pch=16, cex=cex,
             xpd=TRUE)
    }
    return(invisible(NULL))
  }
  else{
    if (missing(xlim)) xlim <- range(x)
    levels <- levels(by)
    n.groups <- length(levels)
    save.par <- par(mfrow=c(n.groups, 1))
    on.exit(par(save.par))
    # the following dummy plot establishes character size
    plot(range(x), 0:1, type="n", axes=FALSE, ann=FALSE,
         xlim=xlim)
    y.limits <- par("usr")[3:4]
    char.height <- correction.height*par("cxy")[2]
    par(mfrow=c(1, 1))
    # the following dummy plot resets the plot device
    plot(0, type="n", axes=FALSE, ann=FALSE)
    par(mfrow=c(n.groups, 1))
    if (bin){
      for(level in levels){
        # compute histograms by level to find maximum count
        max.count <- 0
        hist.level <- hist(x[by == level], breaks=hist$breaks, plot=FALSE)
        max.count <- max(max.count, hist.level$counts)
      }
      cex <- min(((y.limits[2] - y.limits[1])/char.height)/max.count, 2)
      for (level in levels){
        Dotplot(x[by == level], xlab=xlab, main=paste(label.by, "=", level),
                cex=cex, bin=TRUE, breaks=hist$breaks, xlim=xlim, correction=1/4)
      }
    }
    else {
      y <- table(x, by)
      cex <- min(((y.limits[2] - y.limits[1])/char.height)/max(y), 2)
      for (level in levels){
        Dotplot(x[by == level], xlab=xlab, main=paste(label.by, "=", level),
                cex=cex, xlim=xlim, correction=1/5)
      }
    }
  }
}