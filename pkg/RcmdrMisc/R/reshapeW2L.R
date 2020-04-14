reshapeW2L <- function(data, within, levels, varying, ignore, id="id"){
  
  # process variable names
  if (missing(ignore)) ignore <- NULL
  all <- colnames(data)
  use <- setdiff(all, ignore)
  all.varying <- unlist(varying)
  between <- setdiff(use, all.varying)
  levs <- expand.grid(rev(levels))
  
  # create empty output long data set
  m <- nrow(levs)
  n <- nrow(data) * m
  out <- data.frame(id=character(0), stringsAsFactors=FALSE)
  names(out)[1] <- id
  for (bet in between){
    b <- data[[bet]]
    out[[bet]] <- if (is.factor(b)) factor(NULL, levels=levels(b)) else vector(0, mode=mode(b))
  }  
  for (win in within){
    out[[win]] <- factor(NULL, levels[[win]])
  }
  for (var in names(varying)){
    v <- data[[varying[[var]][1]]]
    out[[var]] <- if (is.factor(v)) factor(NULL, levels=levels(v)) else vector(0, mode=mode(v))
  }
  out[1:n, ] <- NA
  
  # fill output data set by cases in the wide data set
  for (i in 1:nrow(data)){
    j <- ((i - 1)*m + 1):(i*m)
    out[j, id] <- as.character(i)
    out[j, between] <- data[i, between]
    out[j, rev(within)] <- levs
    for (var in names(varying)){
      out[j, var] <- unlist(data[i, varying[[var]]])
    }
  }
  
  # create row names
  rownames(out) <- paste0(out[[id]], ".", 1:m)
  
  out
}