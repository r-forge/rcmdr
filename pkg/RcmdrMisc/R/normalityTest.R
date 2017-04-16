normalityTest <- function(x, ...){
  UseMethod("normalityTest")
}

normalityTest.formula <- function(formula, test, data, ...){
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (missing(test)) test <- NULL
  if (ncol(mf) == 1) normalityTest(mf[, 1], test=test, vname=colnames(mf), ...)
  else if (ncol(mf) == 2) normalityTest(mf[, 1], test=test, groups=mf[, 2], vname=colnames(mf)[1], 
                                        gname=colnames(mf)[2], ...)
  else stop("the formula must specify one or two variables")
}

normalityTest.default <- function(x, 
    test=c("shapiro.test", "ad.test", "cvm.test", "lillie.test", "pearson.test", "sf.test"),
    groups, vname, gname, ...){
  test <- match.arg(test)
  if (missing(vname)) vname <- deparse(substitute(x))
  if (missing(groups)){
    result <- do.call(test, list(x=x, ...))
    result$data.name <- vname
    result
  }
  else {
    if (!is.factor(groups)) stop("'groups' must be a factor.")
    {
      if (missing(gname)) gname <- deparse(substitute(groups))
      levels <- levels(groups)
      pvalues <- matrix(0, length(levels), 2)
      rownames(pvalues) <- levels
      cat("\n --------")
      for (level in levels){
        result <- do.call(test, list(x=x[groups == level], ...))
        result$data.name <- vname
        pvalues[level, 1] <- result$p.value
        cat("\n", gname, "=", level, "\n")
        print(result)
        cat(" --------")
      }
      pvalues[, 2] <- p.adjust(pvalues[, 1])
      pvals <- matrix("", length(levels), 2)
      colnames(pvals) <- c("unadjusted", "adjusted")
      rownames(pvals) <- levels
      pvals[, 1] <- format.pval(pvalues[, 1])
      pvals[, 2] <- format.pval(pvalues[, 2])
      cat("\n\n p-values adjusted by the Holm method:\n")
      print(pvals, quote=FALSE)
      return(invisible(NULL))
    }
  }
}