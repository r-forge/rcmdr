discretePlot <- function(x, by, scale=c("frequency", "percent"), xlab=deparse(substitute(x)), ylab=scale, main="", 
                         xlim=NULL, ylim=NULL, ...){
    force(xlab)
    scale <- match.arg(scale)
    dp <- function(x, scale, xlab, ylab, main, xlim, ylim){
        y <- as.vector(table(x))
        if (scale == "percent") y <- 100*y/sum(y)
        x <- sort(unique(x))
        if (is.null(ylim)) ylim <- c(0, max(y, na.rm=TRUE))
        plot(x, y, type=if (min(ylim) == 0) "h" else "n", xlab=xlab, ylab=ylab, main=main, xlim=xlim, ylim=ylim,
             axes=FALSE, frame.plot=TRUE, ...)
        axis(2)
        axis(1, at=x)
        points(x, y, pch=16)
        abline(h=0, col="gray")
    }
    if (is.null(xlim)) xlim <- range(x, na.rm=TRUE)
    if (!is.null(ylim) && min(ylim) != 0) warning("the lower end of the y-axis is not 0")
    if (missing(by)){
        dp(na.omit(x), scale, xlab, ylab, main, xlim, ylim, ...)
    }
    else{
        by.var <- deparse(substitute(by))
        if (!is.factor(by)){
            if (!(is.character(by) || is.logical(by))){
                stop("by must be a factor, character, or logical")
            }
            by <- as.factor(by)
        }
        complete <- complete.cases(x, by)
        x <- x[complete]
        by <- by[complete]
        if (is.null(ylim)){
            max.y <- if (scale == "frequency") max(table(x, by))
                else {
                    tab <- colPercents(table(x, by))
                    max(tab[1:(nrow(tab) - 2), ])
                }
            ylim <- c(0, max.y)
        }
        levels <- levels(by)
        save.par <- par(mfcol=c(length(levels), 1))
        on.exit(par(save.par))
        for (level in levels){
            dp(x[by == level], scale=scale, xlab=xlab, ylab=ylab, main = paste(by.var, "=", level), 
               xlim=xlim, ylim=ylim, ...)
        }
    }
}
