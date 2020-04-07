# last modified: 2020-04-07

repeatedMeasuresPlot <- function(data, within, within.names, within.levels, between.names=NULL,
                                 response.name="score", trace, xvar, col=palette()[-1]){
  
  if (missing(trace)) trace <- NA
  if (missing(xvar)) xvar <- NA

  reshapeW2L <- function(data){
    timevar <- paste(within.names, collapse=".")
    long <- reshape(data, varying=within, v.names=response.name, 
                    timevar=timevar, 
                    direction="long")
    n.levels <- sapply(within.levels, length)
    n.within <- length(within.names)
    if (n.within > 2 || n.within < 1) stop("there must be 1 or 2 within factors")
    if (prod(n.levels) != length(within)){
      stop("the number of repeated measures, ", length(within), 
           ", is not equal to the product of the numbers of levels of the within factors, ",
           prod(n.levels))
    }
    if (length(within.names) != length(within.levels)){
      stop("the number of within factors, ", length(within.names),
           ", is not equal to the number of sets of within-factor levels, ", 
           length(within.levels))
    }
    if (n.within == 2){
      long[[within.names[1]]] <- factor(within.levels[[within.names[1]]][1 + ((long[[timevar]] - 1) %/% n.levels[2])],
                                        levels=within.levels[[within.names[1]]])
      long[[within.names[2]]] <- factor(within.levels[[within.names[2]]][1 + ((long[[timevar]] - 1) %% n.levels[2])],
                                        levels=within.levels[[within.names[2]]])
    } else{
      long[[within.names]] <- factor(within.levels[[1]][long[[timevar]]], 
                                      levels=within.levels[[1]])
    }
    long
  }
  
  computeMeans <- function(data){
    formula <- paste(response.name, " ~", paste(c(within.names, between.names), collapse="+"))
    means <- Tapply(formula, "mean", data=data)
    if(length(dim(means)) > 1){
      means <- as.data.frame(ftable(means))
      names(means)[ncol(means)] <- response.name
    } else {
      means <- data.frame(names(means), means)
      names(means) <- c(within.names, response.name)
    }
    means
  }
  
  rmPlot <- function(data) {
    n.levels <-
      sapply(data[,-ncol(data), drop = FALSE], function(x)
        length(levels(x)))
    n.factors <- length(n.levels)
    fnames <- names(data)[-ncol(data), drop = FALSE]
    trace <- if (is.na(trace)) {
      wnames <- if (!is.na(xvar)) within.names[!(fnames == xvar)] else within.names
      if (length(within.names) == 1 && n.factors > 1) {
        which.min(n.levels[!(fnames == within.names)])
      } else {
        if (length(wnames) > 0) which.min(n.levels[wnames]) else NULL
      }
    } else {
      which(fnames == trace)
    }
    xvar <- if (is.na(xvar)) {
      wnames <- if (!is.na(trace)) within.names[!(fnames == trace)] else within.names
      which.max(n.levels[wnames])
    } else {
      which(fnames == xvar)
    }
    if (n.factors == 1 || trace == xvar) trace <- NULL
    form <- paste(response.name,
                  " ~",
                  fnames[xvar],
                  if (n.factors > 2)
                    "|",
                  paste(fnames[-c(trace, xvar)], collapse = "+"))
    tr.levels <- n.levels[trace]
    if (!is.null(trace)) {
      xyplot(
        as.formula(form),
        groups = if (!is.null(trace))
          data[[trace]]
        else
          1,
        type = "b",
        lty = 1:tr.levels,
        pch = 1:tr.levels,
        col = col[1:tr.levels],
        cex = 1.25,
        strip = function(...)
          strip.default(strip.names = c(TRUE, TRUE), ...),
        data = data,
        ylab = paste("mean", response.name),
        key = if (!is.null(trace))
          list(
            title = fnames[trace],
            cex.title = 1,
            text = list(levels(data[[trace]])),
            lines = list(lty = 1:tr.levels, col = col[1:tr.levels]),
            points = list(
              pch = 1:tr.levels,
              col = col[1:tr.levels],
              cex = 1.25
            )
          )
      )
    } else {
      xyplot(
        as.formula(form),
        type = "b",
        lty = 1,
        pch = 1,
        col = col[1],
        cex = 1.25,
        strip = function(...)
          strip.default(strip.names = c(TRUE, TRUE), ...),
        data = data,
        ylab = paste("mean", response.name)
      )
    }
  }
  
  Long <- reshapeW2L(data)
  Means <- computeMeans(Long)
  rmPlot(Means)
}
