# last modified 2016-08-30 by J. Fox

readStata <- function(file, rownames=FALSE, stringsAsFactors=default.stringsAsFactors(), convert.dates=TRUE){
    Data <- readstata13::read.dta13(file, convert.factors=stringsAsFactors, convert.dates=convert.dates)
    if (rownames){
        check <- length(unique(col1 <- Data[[1]])) == nrow(Data)
        if (!check) warning ("row names are not unique, ignored")
        else {
            rownames(Data) <- col1
            Data[[1]] <- NULL
        }
    }
    if (stringsAsFactors){
        char.cols <- sapply(Data, class) == "character"
        if (any(char.cols)){
            for (col in names(Data)[char.cols]){
                fac <- Data[, col]
                fac[fac == ""] <- NA
                Data[, col] <- as.factor(fac)
            } 
        }
    }
    Data
}
