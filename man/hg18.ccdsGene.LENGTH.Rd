\name{hg18.ccdsGene.LENGTH}
\docType{data}
\alias{hg18.ccdsGene.LENGTH}
\title{Transcript length data for the organism hg}
\description{hg18.ccdsGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the ccdsGene table.

The data file was made by calling downloadLengthFromUCSC(hg18, ccdsGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(hg18.ccdsGene.LENGTH)
head(hg18.ccdsGene.LENGTH)
}
\keyword{datasets}
