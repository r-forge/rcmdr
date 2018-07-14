discretePlot <- function(x, by, scale=c("frequency", "percent"), xlab=deparse(substitute(x)), ylab=scale, main=""){
    force(xlab)
    scale <- match.arg(scale)
    dp <- function(x, scale, xlab, ylab, main, xlim=range(x), ylim=c(0, max(y))){
        y <- as.vector(table(x))
        if (scale == "percent") y <- 100*y/sum(y)
        x <- sort(unique(x))
        plot(x, y, type="h", xlab=xlab, ylab=ylab, main=main, xlim=xlim, ylim=ylim,
             axes=FALSE, frame.plot=TRUE)
        axis(2)
        axis(1, at=x)
        points(x, y, pch=16)
        abline(h=0, col="gray")
    }
    if (missing(by)){
        dp(na.omit(x), scale, xlab, ylab, main)
    }
    else{
        by.var <- deparse(substitute(by))
        complete <- complete.cases(x, by)
        x <- x[complete]
        by <- by[complete]
        max.y <- if (scale == "frequency") max(table(x, by))
            else {
                tab <- colPercents(table(x, by))
                max(tab[1:(nrow(tab) - 2), ])
            }
        xlim  <- range(x)
        levels <- levels(by)
        save.par <- par(mfcol=c(length(levels), 1))
        on.exit(par(save.par))
        for (level in levels){
            dp(x[by == level], scale=scale, xlab=xlab, ylab=ylab, main = paste(by.var, "=", level), 
               xlim=xlim, ylim=c(0, max.y))
        }
    }
}
