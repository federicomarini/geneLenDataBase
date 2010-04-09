\name{downloadLengthFromUCSC}
\Rdversion{1.1}
\alias{downloadLengthFromUCSC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download Transcript Length Data
}
\description{
Attempts to download the length of each transcript for the genome and gene ID specified from the UCSC genome browser.
}
\usage{
downloadLengthFromUCSC(genome, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genome}{
A string identifying the genome that \code{genes} refer to.  For a list of supported organisms see \code{\link{supportedGenomes}}.
}
  \item{id}{
A string identifying the gene identifier used by \code{genes}.  For a list of supported gene identifierst see \code{\link{supportedGeneIDs}}.
}
}
\details{
For each transcript, the UCSC genome browser is used to obtain the exon boundaries.  The length of each transcript is then taken to be the sum of the lengths of all its exons.  Each transcript is then associated with a gene.

The UCSC does not contain length information for all combinations of genome and gene ID listed by \code{\link{supportedGeneIDs}} and \code{\link{supportedGenomes}}.  If \code{downloadLengthFromUCSC} fails because your gene ID format is not supported for the genome you specified, a list of possible ID formats for the specified genome will be listed.
}
\value{
A data.frame containing with three columns, the gene name, transcript identifier and the length of the transcript.  Each row represents one transcript.
}

\author{
Matthew D. Young \email{myoung@wehi.edu.au}
}
\note{
For some genome / gene ID combinations, no gene ID will be provided by UCSC.  In this case, the gene name column is set to \code{NA}.  However, the transcript ID column will always be populated.
}


\seealso{
\code{\link{supportedGenomes}}, \code{\link{supportedGeneIDs}}
}
\examples{
flat_length <- downloadLengthFromUCSC('hg19', 'ensGene')
}
