# last modified 2017-01-13 by J. Fox

readSPSS <- function(file, rownames=FALSE, stringsAsFactors=default.stringsAsFactors(), tolower=TRUE){
    Data <- as.data.frame(haven::read_spss(file))
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
    num.cols <- sapply(Data, is.numeric)
    if (any(num.cols)){
        for (col in names(Data)[num.cols]) {
            Data[, col] <- as.numeric(Data[, col])
            Data[!is.finite(Data[, col]), col] <- NA
        }
    }
    if (tolower){
        names(Data) <- tolower(names(Data))
    }
    Data
}
