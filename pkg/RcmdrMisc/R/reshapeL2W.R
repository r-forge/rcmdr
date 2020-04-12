reshapeL2W <- function(data, within, id, varying, ignore){

  # create wide data set
  if (missing(ignore)) ignore <- NULL
  names <- colnames(data)
  all <- c(within, id, varying, ignore)
  bad <- all[!all %in% names]
  if (length(bad) > 0) stop("variables not in the data set: ", bad)
  duplicated <- unique(all[duplicated(all)])
  if (length(duplicated) > 0) stop(paste0("the following variables appear more than once: ", paste(duplicated, collapse=", ")))
  if (!is.null(ignore)){
    remove <- which(names(data) %in% ignore )
    data <- data[, -remove]
  }
  within.factors <- data[, within, drop=FALSE]
  within.var <- apply(within.factors, 1, function(x) paste(as.character(x), collapse="."))
  data <- cbind(data, within.var)
  occasions <- paste(within, collapse=".")
  names(data)[length(data)] <- occasions
  occasions.1 <- paste0(occasions, ".1")
  result <- reshape(data, timevar=occasions, idvar=id, v.names=varying,  direction="wide", 
                    drop=if (length(within) > 1) within)
  
  # create names for the repeated-measures columns
  rownames(result) <- result[, id]
  result <- result[, - which(colnames(result) %in% c(id, occasions.1))]
  within.levels <- lapply(within.factors[, rev(within), drop=FALSE], levels)
  grid <- expand.grid(within.levels)
  repeated.names <- apply(grid, 1, function(x) paste(rev(x), collapse="."))
  all.repeated.cols <- NULL
  for (var in varying){
    repeated.cols <- grep(paste0("^", var, "."), names(result))
    nms <- if (length(varying) > 1) paste0(repeated.names, ".", var) else repeated.names
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
