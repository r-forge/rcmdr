# last modified 2020-05-11 by J. Fox

readXL <- function(file, rownames=FALSE, header=TRUE, na="", sheet=1, 
                   stringsAsFactors=FALSE){
  Data <- readxl::read_excel(path=file, sheet=sheet, col_names=header, na=na)
  class(Data) <- "data.frame"
  if (rownames){
      check <- length(unique(col1 <- Data[[1]])) == nrow(Data)
      if (!check) warning ("row names are not unique, ignored")
      else {
          rownames(Data) <- col1
          Data[[1]] <- NULL
      }
  }
  colnames(Data) <- make.names(colnames(Data), unique=TRUE)
  if (stringsAsFactors){
    char <- sapply(Data, class) == "character"
    for (var in which(char)){
      Data[[var]] <- factor(Data[[var]])
    }
  }
  Data
}
