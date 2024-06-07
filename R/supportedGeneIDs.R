#' Supported Gene IDs
#' 
#' Lists supported gene ID formats
#' 
#' Uses the \code{supportedUCSCtables} function from the \code{GenomicFeatures}
#' package to obtain a list of gene ID formats available from the UCSC genome
#' browser.  The \code{db} column gives the gene ID formats which are provided
#' to the \code{id} arguement of various functions.  The track and subtrack
#' columns are the names of the UCSC track/subtrack from which information is
#' fetched.
#' 
#' The \code{GeneID} column lists the "full name" of the gene ID format where
#' available.
#' 
#' The final column, headed \code{AvailableGenomes} lists the genomes for which
#' there is a local copy of the length information avaible for the gene ID
#' format listed in the \code{geneLenDataBase} package.
#' 
#' @return A data.frame containing supported gene ID formats.
#' 
#' @export
#' 
#' @author Matthew D. Young \email{myoung@@wehi.edu.au}
#' 
#' @examples
#' supportedGeneIDs()
#' 
supportedGeneIDs <- function() {
  base <- unfactor(txdbmaker::supportedUCSCtables())

  # Current version
  txname2gene_mapinfo <- txdbmaker:::.UCSC_TXNAME2GENEID_MAPDEFS
  base$GeneID <- ""

  # Old version
  # tmp=unlist(lapply(txname2gene_mapinfo[base$db],function(u){u[[2]]}))
  # base$GeneID[match(names(tmp),base$db)]=as.character(tmp)

  # NEW Ant 28/09/16
  tmp <- unlist(lapply(txname2gene_mapinfo[base$tablename], function(u) {
    u[[2]]
  }))
  base$GeneID[match(names(tmp), base$tablename)] <- as.character(tmp)

  # Add in gene symbol
  base[nrow(base) + 1, ] <- c("geneSymbol", "refGene", "refFlat", "Gene Symbol")
  base$AvailableGenomes <- ""
  repo <- grep(".*\\..*\\.LENGTH", as.data.frame(data(package = "geneLenDataBase")$results, stringsAsFactors = FALSE)$Item, ignore.case = TRUE, value = TRUE)
  repo <- matrix(unlist(strsplit(repo, "\\.")), ncol = 3, byrow = TRUE)
  valid_genomes <- sapply(split(repo[, 1], repo[, 2]), paste, collapse = ",")
  valid_genomes <- valid_genomes[which(names(valid_genomes) %in% base$tablename)]
  base$AvailableGenomes[match(names(valid_genomes), base$tablename)] <- as.character(valid_genomes)

  avail <- base[base$tablename == "knownGene" | base$tablename == "vegaGene" | base$tablename == "geneSymbol" | base$tablename == "ensGene", ]
  avail
}
