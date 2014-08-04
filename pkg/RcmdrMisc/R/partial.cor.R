# last modified 2014-08-04 by J. Fox

partial.cor <- function(X, ...){
    R <- cor(X, ...)
    RI <- solve(R)
    D <- 1/sqrt(diag(RI))
    R <- - RI * (D %o% D)
    diag(R) <- 0
    rownames(R) <- colnames(R) <- colnames(X)
    R
}
