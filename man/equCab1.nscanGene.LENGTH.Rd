\name{equCab1.nscanGene.LENGTH}
\title{Transcript length data for the organism equCab}
\description{equCab1.nscanGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the nscanGene table.

The data file was made by calling downloadLengthFromUCSC(equCab1, nscanGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(equCab1.nscanGene.LENGTH)
head(equCab1.nscanGene.LENGTH)
}
\keyword{datasets}
