# last modified 2016-07-14 by J. Fox

dgumbel <- function(x, location=0, scale=1){
    z <- (x - location)/scale
    d <- exp(-exp(-z))*exp(-z)/scale
    d[z == -Inf] <- 0
    d
}

pgumbel <- function(q, location=0, scale=1, lower.tail=TRUE){
    p <- exp(-exp(- (q - location)/scale))
    if (lower.tail) p else 1 - p
}

qgumbel <- function(p, location=0, scale=1, lower.tail=TRUE){
    if (!lower.tail) p <- 1 - p
    location - scale*log(-log(p))
}

rgumbel <- function(n, location=0, scale=1){
    location - scale*log(-log(runif(n)))
}
