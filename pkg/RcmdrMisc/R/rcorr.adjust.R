# the following function is adapted from a suggestion by Robert Muenchen

# uses rcorr in the Hmisc package

# last modified 2014-08-04 by J. Fox

rcorr.adjust <- function(x, type=c("pearson", "spearman"), 
    use=c("complete.obs", "pairwise.complete.obs")){
    type <- match.arg(type)
    use <- match.arg(use)
    x <- if (use == "complete.obs") as.matrix(na.omit(x)) else as.matrix(x)
    R <- rcorr(x, type=type)
    P <- R$P
    p <- P[lower.tri(P)]
    adj.p <- p.adjust(p, method="holm")
    P[lower.tri(P)] <- adj.p
    P[upper.tri(P)] <- 0
    P <- P + t(P)
    P <- ifelse(P < 1e-04, 0, P)
    P <- format(round(P, 4))
    P[grep("NA", P)] <- ""
    res <- list(R=R, P=P)
    class(res) <- "rcorr.adjust"
    res
}

print.rcorr.adjust <- function(x, ...){
    print(x$R)
    cat("\n Adjusted p-values (Holm's method)\n")
    print(x$P, quote = FALSE)
}
