#' Supported Genomes
#' 
#' Lists supported genomes
#' 
#' Uses the \code{ucscGenomes()} function from the \code{rtracklayer} package
#' to obtain a list of genomes available from the UCSC genome browser.  The
#' \code{db} column lists genomes as they are provided to the \code{genome}
#' arguement of various functions.
#' 
#' The final column, headed \code{AvailableGeneIDs} lists the gene ID formats
#' for which there is a local copy of the length information avaible for the
#' genome listed in the \code{geneLenDataBase} package.
#' 
#' @return A data.frame containing supported genomes.
#' 
#' @export
#' 
#' @author Matthew D. Young \email{myoung@@wehi.edu.au}
#' 
#' @examples
#' supportedGenomes()
#' 
supportedGenomes <- function() {
  base <- unfactor(ucscGenomes())
  base$AvailableGeneIDs <- ""
  repo <- grep(".*\\..*\\.LENGTH", as.data.frame(data(package = "geneLenDataBase")$results, stringsAsFactors = FALSE)$Item, ignore.case = TRUE, value = TRUE)
  repo <- matrix(unlist(strsplit(repo, "\\.")), ncol = 3, byrow = TRUE)
  valid_ids <- sapply(split(repo[, 2], repo[, 1]), paste, collapse = ",")
  valid_ids <- valid_ids[which(names(valid_ids) %in% base$db)]
  base$AvailableGeneIDs[match(names(valid_ids), base$db)] <- as.character(valid_ids)
  avail <- base[base$AvailableGeneIDs != "", ]
  avail
}
