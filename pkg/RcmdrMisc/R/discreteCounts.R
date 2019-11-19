discreteCounts <- function(x, round.percents=2, name=deparse(substitute(x)), 
                           max.values=min(round(2*sqrt(length(x))), 100)){
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.matrix(x)) {
        names <- colnames(x)
        for (j in 1:ncol(x)){
            discreteCounts(x[, j], round.percents=round.percents, name=names[j], max.values=max.values)
            cat("\n")
        }
        return(invisible(NULL))
    }
    Count <- table(x)
    if ((nv <- length(Count)) > max.values) stop("number of unique values of ", name, ", ", nv, ", exceeds maximum, ", max.values)
    tot <- sum(Count)
    Percent <- round(100*Count/tot, round.percents)
    tot.percent <- round(sum(Percent), round.percents)
    table <- cbind(Count, Percent)
    table <- rbind(table, c(tot, tot.percent))
    rownames(table) <- c(names(Count), "Total")
    cat("Distribution of", name, "\n")
    print(table)
    return(invisible(Count))
}
