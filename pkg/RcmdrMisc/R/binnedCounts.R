binnedCounts <- function(x, breaks="Sturges", name=deparse(substitute(x))){
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.matrix(x)) {
        names <- colnames(x)
        for (j in 1:ncol(x)){
            binnedCounts(x[, j], breaks=breaks, name=names[j])
            cat("\n")
        }
        return(invisible(NULL))
    }
    dist <- hist(x, breaks=breaks, plot=FALSE)
    counts <- dist$counts
    breaks <- dist$breaks
    names(counts) <- paste0("(", breaks[1:(length(breaks) - 1)], ", ", breaks[-1], "]")
    cat("distribution of", name, "\n")
    print(counts)
    return(invisible(counts))
}
