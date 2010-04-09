\name{supportedGeneIDs}
\Rdversion{1.1}
\alias{supportedGeneIDs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Supported Gene IDs
}
\description{
Lists supported gene ID formats
}
\usage{
supportedGeneIDs()
}
\details{
Uses the \code{supportedUCSCtables} function from the \code{GenomicFeatures} package to obtain a list of gene ID formats available from the UCSC genome browser.  The \code{db} column gives the gene ID formats which are provided to the \code{id} arguement of various functions.  The track and subtrack columns are the names of the UCSC track/subtrack from which information is fetched.  

The \code{GeneID} column lists the "full name" of the gene ID format where available.

The final column, headed \code{AvailableGenomes} lists the genomes for which there is a local copy of the length information avaible for the gene ID format listed in the \code{geneLenDataBase} package.
}
\value{
A data.frame containing supported gene ID formats.
}

\author{
Matthew D. Young \email{myoung@wehi.edu.au}
}
\examples{
supportedGeneIDs()
}
