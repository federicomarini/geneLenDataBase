\name{ce4.xenoRefGene.LENGTH}
\title{Transcript length data for the organism ce}
\description{ce4.xenoRefGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the xenoRefGene table.

The data file was made by calling downloadLengthFromUCSC(ce4, xenoRefGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(ce4.xenoRefGene.LENGTH)
head(ce4.xenoRefGene.LENGTH)
}
\keyword{datasets}
