\name{dp3.xenoRefGene.LENGTH}
\docType{data}
\alias{dp3.xenoRefGene.LENGTH}
\title{Transcript length data for the organism dp}
\description{dp3.xenoRefGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the xenoRefGene table.

The data file was made by calling downloadLengthFromUCSC(dp3, xenoRefGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(dp3.xenoRefGene.LENGTH)
head(dp3.xenoRefGene.LENGTH)
}
\keyword{datasets}
