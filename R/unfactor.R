#' Purge factors
#' 
#' Removes all factors from a variable in a sensible way.
#' 
#' As factors are their own type, to remove factors we must convert each level
#' into another type.  This is currently done using "typeless" behaviour: a
#' factor is converted to a numeric vector if this can be done without inducing
#' NAs, otherwise it is coerced using as.character. Currently supported types
#' are: \code{factor}, \code{data.frame} and \code{list}.
#' 
#' @param var The variable from which you want the factors removed.
#' @return The variable with all factors converted to characters or numbers
#' (see \code{details}).
#' 
#' @export
#' 
#' @author Matthew D. Young \email{myoung@@wehi.edu.au}
#' 
#' @examples
#' # A named factor
#' x <- factor(sample(1:6, 100, replace=TRUE))
#' names(x) <- paste("Roll.No", 1:100, sep='.')
#' x
#' unfactor(x)
#' 
#' # A data.frame
#' x <- data.frame(player <- c("Alice", "Bob", "Mary", "Fred"), 
#'                 score <- factor(c(9, 7, 8, 9)), stringsAsFactors=TRUE)
#' x$player
#' x$score
#' y <- unfactor(x)
#' y$player
#' y$score
#' 
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
