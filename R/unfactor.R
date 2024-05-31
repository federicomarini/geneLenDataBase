unfactor <- function(var) {
  if (is.factor(var)) {
    tmp <- names(var)
    tmpopt <- getOption("warn")
    options(warn = -1)
    out <- as.numeric(levels(var))[as.integer(var)]
    options(warn = tmpopt)
    if (any(is.na(out)) & any(is.na(out) != is.na(var))) {
      out <- as.character(levels(var))[as.integer(var)]
    }
    names(out) <- tmp
  } else if (is.data.frame(var)) {
    # Have to use a loop, since calling apply will return a matrix
    out <- var
    for (i in 1:dim(var)[2]) {
      out[, i] <- unfactor(var[, i])
    }
  } else if (is.list(var)) {
    # Mmmmm, recursion
    out <- lapply(var, unfactor)
  } else {
    # The default option
    out <- var
  }
  return(out)
}
