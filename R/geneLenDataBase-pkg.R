#' geneLenDatabase: 
#'
#' Lengths of mRNA transcripts for a number of genomes
#'
#' Length of mRNA transcripts for a number of genomes and gene ID formats, 
#' largely based on UCSC table browser.
#' Data objects are provided as individual pieces of information to be retrieved 
#' and loaded. 
#' A variety of different gene identifiers and genomes is supported to ensure 
#' wide applicability.
#' 
#' @importFrom utils data
#' @importFrom rtracklayer ucscGenomes browserSession genome "genome<-" 
#' ucscTableQuery getTable trackNames
#' @importFrom GenomicFeatures transcriptWidths
#' @importFrom txdbmaker supportedUCSCtables
#'
#' @name geneLenDatabase-pkg
#' @docType package
#' @keywords internal
"_PACKAGE"


