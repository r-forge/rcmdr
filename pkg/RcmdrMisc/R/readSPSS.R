# last modified 2017-01-20 by J. Fox

readSPSS <- function(file, rownames=FALSE, stringsAsFactors=default.stringsAsFactors(), tolower=TRUE, 
                     use.haven=!por){
    filename <- rev(strsplit(file, "\\.")[[1]])
    por <- "por" == if (length(filename) > 1) filename[1] else ""
    Data <- if (use.haven) as.data.frame(haven::read_spss(file))
            else foreign::read.spss(file, to.data.frame=TRUE)
    if (rownames){
        col1 <- gsub("^\ *", "", gsub("\ *$", "", Data[[1]]))
        check <- length(unique(col1)) == nrow(Data)
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
    if (use.haven && any(num.cols)){
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
