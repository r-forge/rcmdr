binnedCounts <- function(x, breaks="Sturges", round.percents=2, name=deparse(substitute(x))){
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
    Count <- dist$counts
    breaks <- dist$breaks
    tot <- sum(Count)
    Percent <- round(100*Count/tot, round.percents)
    tot.percent <- round(sum(Percent), round.percents)
    names(Count) <- paste0(c("[", rep("(", length(breaks) - 2)), breaks[1:(length(breaks) - 1)], ", ", breaks[-1], "]")
    table <- cbind(Count, Percent)
    table <- rbind(table, c(tot, tot.percent))
    rownames(table)[nrow(table)] <- "Total"
    cat("Binned distribution of", name, "\n")
    print(table)
    return(invisible(Count))
}
