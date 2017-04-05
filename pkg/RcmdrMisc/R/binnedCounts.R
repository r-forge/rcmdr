binnedCounts <- function(x, breaks="Sturges"){
    dist <- hist(x, breaks=breaks, plot=FALSE)
    counts <- dist$counts
    breaks <- dist$breaks
    names(counts) <- paste0(breaks[1:(length(breaks) - 1)], ":", breaks[-1])
    counts
}
