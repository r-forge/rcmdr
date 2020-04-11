reshapeL2W <- function(data, within.names, id, responses){
  
  # data: long data frame
  # within.names: names of columns corresponding to repeated-measures factor(s)
  # id: name of column identifying subjects
  # responses: names of columns holding the response(s)
  
  # create wide data set
  names <- colnames(data)
  all <- c(within.names, id, responses)
  bad <- all[!all %in% names]
  if (length(bad) > 0) stop("variables not in the data set: ", bad)
  within <- data[, within.names, drop=FALSE]
  within.var <- apply(within, 1, function(x) paste(as.character(x), collapse="."))
  data <- cbind(data, within.var)
  occasions <- paste(within.names, collapse=".")
  names(data)[length(data)] <- occasions
  occasions.1 <- paste0(occasions, ".1")
  result <- reshape(data, timevar=occasions, idvar=id, v.names=responses,  direction="wide", 
                    drop=if (length(within.names) > 1) within.names)
  
  # create names for the repeated-measures columns
  rownames(result) <- result[, id]
  result <- result[, - which(colnames(result) %in% c(id, occasions.1))]
  within.levels <- lapply(within[, rev(within.names), drop=FALSE], levels)
  grid <- expand.grid(within.levels)
  repeated.names <- apply(grid, 1, function(x) paste(rev(x), collapse="."))
  all.repeated.cols <- NULL
  for (var in responses){
    repeated.cols <- grep(paste0("^", var, "."), names(result))
    nms <- if (length(responses) > 1) paste0(repeated.names, ".", var) else repeated.names
    names(result)[repeated.cols] <- make.names(nms)
    all.repeated.cols <- c(all.repeated.cols, repeated.cols)
  }
  
  # remove cases with incomplete repeated measures
  bad <- apply(result[, all.repeated.cols], 1, function(x) anyNA(x))
  n.bad <- sum(bad)
  if (n.bad > 0){
    warning(n.bad, " ", if (n.bad == 1) "case" else "cases",  
            " removed due to missing repeated measures")
    result <- result[!bad, ]
  }
  
  result
}
